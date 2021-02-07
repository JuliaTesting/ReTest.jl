module ReTest

export retest, @testset

using Distributed
using Base.Threads: nthreads
using Random: shuffle!

# from Test:
export Test,
    @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated,
    @inferred,
    detect_ambiguities, detect_unbound_args

using Test: Test,
    @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated,
    @inferred,
    detect_ambiguities, detect_unbound_args

using InlineTest: @testset, InlineTest, TESTED_MODULES, INLINE_TEST
import InlineTest: retest

include("testset.jl")

using .Testset: Testset, Format

Base.@kwdef mutable struct Options
    verbose::Bool = false # annotated verbosity
    transient_verbose::Bool = false # verbosity for next run
end

mutable struct TestsetExpr
    source::LineNumberNode
    mod::String # enclosing module
    desc::Union{String,Expr}
    options::Options
    loops::Union{Expr,Nothing}
    parent::Union{TestsetExpr,Nothing}
    children::Vector{TestsetExpr}
    strings::Vector{String}
    loopvalues::Any
    hasbroken::Bool
    hasbrokenrec::Bool # recursive hasbroken, transiently
    run::Bool
    descwidth::Int # max width of self and children shown descriptions
    body::Expr

    TestsetExpr(source, mod, desc, options, loops, parent, children=TestsetExpr[]) =
        new(source, mod, desc, options, loops, parent, children, String[])
end

isfor(ts::TestsetExpr) = ts.loops !== nothing
isfinal(ts::TestsetExpr) = isempty(ts.children)

# replace unqualified `@testset` by TestsetExpr
function replace_ts(source, mod, x::Expr, parent)
    if x.head === :macrocall && x.args[1] === Symbol("@testset")
        @assert x.args[2] isa LineNumberNode
        ts, hasbroken = parse_ts(source, mod, Tuple(x.args[3:end]), parent)
        parent !== nothing && push!(parent.children, ts)
        ts, false # hasbroken counts only "proper" @test_broken, not recursive ones
    elseif x.head === :macrocall && x.args[1] === Symbol("@test_broken")
        x, true
    else
        body_br = map(z -> replace_ts(source, mod, z, parent), x.args)
        Expr(x.head, first.(body_br)...), any(last.(body_br))
    end
end

replace_ts(source, mod, x, _) = x, false

# create a TestsetExpr from @testset's args
function parse_ts(source, mod, args::Tuple, parent=nothing)
    local desc
    options = Options()
    for arg in args[1:end-1]
        if arg isa String || Meta.isexpr(arg, :string)
            desc = arg
        elseif Meta.isexpr(arg, :(=))
            arg.args[1] in fieldnames(Options) || error("unsupported @testset option")
            # TODO: make that work with non-literals:
            setfield!(options, arg.args[1], arg.args[2])
        else
            error("unsupported @testset")
        end
    end
    @isdefined(desc) || error("@testset requires a description")

    body = args[end]
    isa(body, Expr) || error("Expected begin/end block or for loop as argument to @testset")
    if body.head === :for
        loops = body.args[1]
        tsbody = body.args[2]
    elseif body.head === :block
        loops = nothing
        tsbody = body
    else
        error("Expected begin/end block or for loop as argument to @testset")
    end

    ts = TestsetExpr(source, mod, desc, options, loops, parent)
    ts.body, ts.hasbroken = replace_ts(source, mod, tsbody, ts)
    ts, false # hasbroken counts only "proper" @test_broken, not recursive ones
end

function resolve!(mod::Module, ts::TestsetExpr, rx::Regex;
                  force::Bool=false, shown::Bool=true, depth::Int=0,
                  verbose::Int)
    strings = empty!(ts.strings)
    desc = ts.desc
    ts.run = force || isempty(rx.pattern)
    ts.loopvalues = nothing # unnecessary ?

    parentstrs = ts.parent === nothing ? [""] : ts.parent.strings
    ts.descwidth = 0
    ts.options.transient_verbose = shown & ((verbose > 1) | ts.options.verbose)

    if desc isa String
        if shown
            ts.descwidth = textwidth(desc) + 2*depth
        end
        for str in parentstrs
            ts.run && break
            new = str * '/' * desc
            if occursin(rx, new)
                ts.run = true
            else
                push!(strings, new)
            end
        end
    else
        loops = ts.loops
        @assert loops !== nothing
        xs = ()
        try
            xs = Core.eval(mod, loops.args[2])
            if !(xs isa Union{Array,Tuple}) # being conservative on target type
                # this catches e.g. the case where xs is a generator, then collect
                # fails because of a world-age problem (the function in xs is too "new")
                xs = collect(xs)
            end
            ts.loopvalues = xs
        catch
            xs = () # xs might have been assigned before the collect call
            if !ts.run
                @warn "could not evaluate testset-for iterator, default to inclusion"
            end
            ts.run = true
            if shown
                # set ts.descwidth to a lower bound to reduce misalignment
                ts.descwidth = 2*depth + mapreduce(textwidth, +,
                                                   filter(x -> x isa String, desc.args))
            end
        end
        for x in xs # empty loop if eval above threw
            Core.eval(mod, Expr(:(=), loops.args[1], x))
            descx = Core.eval(mod, desc)::String
            if shown
                ts.descwidth = max(ts.descwidth, textwidth(descx) + 2*depth)
            end
            if ts.run
                if !shown # no need to compute subsequent descx to update ts.descwidth
                    break
                else
                    continue
                end
            end
            for str in parentstrs
                new = str * '/' * descx
                if occursin(rx, new)
                    ts.run = true
                    break
                else
                    push!(strings, new)
                end
            end
        end
    end

    run = ts.run
    ts.hasbrokenrec = ts.hasbroken
    for tsc in ts.children
        run |= resolve!(mod, tsc, rx, force=ts.run,
                        shown=shown & ts.options.transient_verbose,
                        depth=depth+1, verbose=verbose-1)
        ts.descwidth = max(ts.descwidth, tsc.descwidth)
        if tsc.run
            ts.hasbrokenrec |= tsc.hasbrokenrec
        end
    end
    if !run || verbose <= 0
        ts.descwidth = 0
    end
    ts.run = run
end

# convert a TestsetExpr into an actually runnable testset
function make_ts(ts::TestsetExpr, rx::Regex, stats, chan)
    ts.run || return nothing

    if isfinal(ts)
        body = ts.body
    else
        body = make_ts(ts.body, rx, stats, chan)
    end
    if ts.loops === nothing
        quote
            @testset $(ts.mod) $(isfinal(ts)) $rx $(ts.desc) $(ts.options) $stats $chan $body
        end
    else
        loopvals = something(ts.loopvalues, ts.loops.args[2])
        quote
            @testset $(ts.mod) $(isfinal(ts)) $rx $(ts.desc) $(ts.options) $stats $chan $(ts.loops.args[1]) $loopvals $body
        end
    end
end

make_ts(x, rx, _, _) = x
make_ts(ex::Expr, rx, stats, chan) =
    Expr(ex.head, map(x -> make_ts(x, rx, stats, chan), ex.args)...)

# convert raw tests from InlineTest into TestsetExpr tests, and handle overwriting
function updatetests!(mod::Module)
    tests, news, map = InlineTest.get_tests(mod)
    # work-around lack of ordered-dict
    # map: we keep only the latest version of a test at a given location,
    #      to be Revise-friendly (just an imperfect heuristic)
    for (tsargs, source) in news
        ts, hasbroken = parse_ts(source, string(mod), tsargs)
        idx = get!(map, ts.desc, length(tests) + 1)
        if idx == length(tests) + 1
            push!(tests, ts)
        else
            revise = Base.PkgId(Base.UUID("295af30f-e4ad-537b-8983-00126c2a3abe"), "Revise")
            if !(revise in keys(Base.loaded_modules))
                desc = ts.desc isa String ? string('"', ts.desc, '"') : ts.desc
                source = string(ts.source.file, ':', ts.source.line)
                @warn "duplicate description for @testset, overwriting: $desc at $source"
            end
            tests[idx] = ts
        end
    end
    empty!(news)
    tests
end

"""
    retest([m::Module...], pattern = r""; dry::Bool=false, stats::Bool=false,
                                          shuffle::Bool=false, verbose::Real=true)

Run all the tests declared in `@testset` blocks, within modules `m` if specified,
or within all currently loaded modules otherwise.
If `dry` is `true`, don't actually run the tests, just print the descriptions
of the testsets which would (presumably) run.
If `stats` is `true`, print some time/memory statistics for each testset.
If `shuffle` is `true`, shuffle the order in which top-level testsets within
a given module are run, as well as the list of passed modules.
If specified, `verbose` must be an integer or `Inf` indicating the nesting level
of testsets whose results must be printed (this is equivalent to adding the
`verbose=true` annotation to corresponding testsets); the default behavior
(`true` or `1`) corresponds to printing the result of top-level testsets.

It's possible to filter run testsets by specifying `pattern`: the "subject" of a
testset is the concatenation of the subject of its parent `@testset`, if any,
with `"/\$description"` where `description` is the testset's description.
For example:
```julia
@testset "a" begin # subject == "/a"
    @testset "b" begin # subject is "/a/b"
    end
    @testset "c\$i" for i=1:2 # subjects are "/a/c1" & "/a/c2"
    end
end
```
A testset is guaranteed to run only when its subject matches `pattern`.
Moreover if a testset is run, its enclosing testset, if any, also has to run
(although not necessarily exhaustively, i.e. other nested testsets
might be filtered out).

If the passed `pattern` is a string, then it is wrapped in a `Regex` with the
"case-insensitive" flag, and must match literally the subjects.
This means for example that `"a|b"` will match a subject like `"a|b"` or `"A|B"`,
but not like `"a"` (only in Julia versions >= 1.3; in older versions,
the regex is simply created as `Regex(pattern, "i")`).

Note: this function executes each (top-level) `@testset` block using `eval` *within* the
module in which it was written (e.g. `m`, when specified).
"""
function retest(args::Union{Module,AbstractString,Regex}...;
                dry::Bool=false,
                stats::Bool=false,
                shuffle::Bool=false,
                group::Bool=true,
                verbose::Real=true, # should be @nospecialize, but not supported on old Julia
                )

    modules, regex, verbose = process_args(args, verbose, shuffle)
    overall = length(modules) > 1
    root = Testset.ReTestSet("", "Overall", true)

    tests_descs_hasbrokens = fetchtests.(modules, regex, verbose, overall)
    isempty(tests_descs_hasbrokens) &&
        throw(ArgumentError("no modules using ReTest could be found and none were passed"))

    alltests = first.(tests_descs_hasbrokens)
    descwidth = max(textwidth(root.description),
                    maximum(x->x[2], tests_descs_hasbrokens))
    format = Format(stats, descwidth)
    hasbroken = any(last.(tests_descs_hasbrokens))

    emptymods = findall(isempty, alltests)
    nmodules = length(modules) - length(emptymods)
    if nmodules == 0
        plural = length(emptymods) > 1 ? "s" : ""
        print("No matching tests for module$plural ")
        join(stdout, string.(getindex.((modules,), emptymods)), ", ", " and ")
        println('.')
        return
    end

    for imod in eachindex(modules)
        mod = modules[imod]
        tests = alltests[imod]
        isempty(tests) && continue

        shuffle &&
            shuffle!(tests)

        if dry
            overall &&
                println(mod)
            foreach(ts -> dryrun(mod, ts, regex, overall*2), tests)
            continue
        end

        if group && nworkers() > 1
            # make test groups according to file names
            files = Dict{Symbol, Int}()
            n = 1
            for ts in tests
                k = get!(files, ts.source.file, n)
                n += (k == n)
            end

            sort!(tests, lt = function(s, t)
                      files[s.source.file] < files[t.source.file]
                  end)

            groups = [1 => tests[1].source.file]
            for (ith, ts) in enumerate(tests)
                _, file = groups[end]
                if ts.source.file != file
                    push!(groups, ith => ts.source.file)
                end
            end
            todo = fill(true, length(tests))
        end

        outchan = RemoteChannel(() -> Channel{Union{Nothing,Testset.ReTestSet}}(0))
        computechan = nprocs() == 1 ?
            Channel{Nothing}(1) : # to not interrupt printer task
            nothing

        ntests = 0
        nprinted = 0
        allpass = true
        exception = Ref{Exception}()
        interrupted = Threads.Atomic{Bool}(false)

        module_ts = Testset.ReTestSet("", string(mod) * ':', true)
        push!(root.results, module_ts)

        many = length(tests) > 1 || isfor(tests[1]) # FIXME: isfor when only one iteration

        printlock = ReentrantLock()
        previewchan =
            if stdout isa Base.TTY && (nthreads() > 1 || nprocs() > 1)
                RemoteChannel(() -> Channel{Union{String,Nothing}}(Inf))
                # needs to be "remote" in the case nprocs() == 2, as then nworkers() == 1,
                # which means the one remote worker will put descriptions on previewchan
                # (if nworkers() > 1, descriptions are not put because we can't predict
                # the order in which they complete, and then the previewer will
                # not show the descriptions, just the spinning wheel)

                # channel size: if nworkers() == 1, then 2 would suffice (one for
                # the "compilation step", one for @testset execution step, and then
                # the printer would empty the channel; but for two workers and more,
                # this second step is not done, so the buffer needs a size of at least
                # `nworkers()`
            else
                # otherwise, the previewing doesn't work well, because the worker task
                # keeps the thread busy and doesn't yield enough for previewing to be useful
                nothing
            end

        gotprinted = false
        align_overflow = 0

        function take_latest!(previewchan)
            local desc
            while isready(previewchan)
                # printer/previewer can't take! it, as we locked
                desc = take!(previewchan)
            end
            @isdefined(desc) ? desc : ""
        end

        previewer = previewchan === nothing ? nothing :
            @async try
                timer = ['|', '/', '-', '\\']
                cursor = 0
                desc = ""
                finito = false

                while !finito && !interrupted[]
                    lock(printlock) do
                        newdesc = take_latest!(previewchan)
                        if newdesc === nothing
                            finito = true
                            return # no need to sleep before looping
                        elseif newdesc != ""
                            desc = newdesc
                            cursor = 0
                            gotprinted = false
                        elseif gotprinted
                            desc = ""
                            gotprinted = false
                            align_overflow = 0
                        elseif desc != ""
                            align = format.desc_align
                            if nworkers() > 1
                                description = align >= 3 ? "..." : ""
                                style = NamedTuple()
                            elseif startswith(desc, '\0')
                                description = chop(desc, head=1, tail=0)
                                style = (color = :light_black, bold=true)
                            else
                                description = desc
                                style = NamedTuple()
                            end
                            if isindented(verbose, overall, many)
                                description = "  " * description
                            end
                            cursor += 1

                            # when verbose == 0, we still can print the currently run
                            # testset, but then its description might be larger than
                            # `align`, because it was not taken into account for computing
                            # `align`;
                            # `align_overflow` computes how many characters do overflow,
                            # so that the printer can "erase" them later on;
                            # once we overflow, we don't go back (leftwards) until the
                            # printer prints
                            align_overflow =
                                max(align_overflow, textwidth(description) - align)
                            printstyled('\r',
                                        rpad("$description", align+align_overflow, " "),
                                        ' ',
                                        timer[mod1(cursor, end)];
                                        style...)
                        end
                    end
                    sleep(0.13)
                end
            catch ex
                # TODO: clarify what is the correct thing to do here
                if ex isa InterruptException
                    interrupted[] = true
                    rethrow()
                else
                    # then there is probably a bug in the previewer code, but it might be fine
                    # for the worker/printer to continue?
                    rethrow()
                end
            end # previewer task

        # TODO: move printer task out of worker?
        worker = @task begin
            printer = @async begin
                errored = false
                finito = false

                print_overall() =
                    if many || verbose == 0
                        @assert endswith(module_ts.description, ':')
                        module_ts.description = chop(module_ts.description, tail=1)
                        clear_line()
                        Testset.print_test_results(module_ts, format,
                                                   bold=true, hasbroken=hasbroken)
                    else
                        nothing
                    end

                # if the previewer overflowed, we must clear the line, otherwise, if
                # what we print now isn't as large, leftovers from the previewer
                # will be seen
                clear_line() = if previewchan !== nothing
                    # +2: for the final space before spinning wheel and the wheel
                    print('\r' * ' '^(format.desc_align+align_overflow+2) * '\r')
                    align_overflow = 0
                end

                while !finito && !interrupted[]
                    rts = take!(outchan)
                    lock(printlock) do
                        if previewchan !== nothing
                            desc = take_latest!(previewchan)
                            if desc === nothing
                                # keep `nothing` in so that the previewer knows to terminate
                                put!(previewchan, nothing)
                            end
                        end
                        gotprinted = true

                        if rts === nothing
                            errored || print_overall()
                            finito = true
                            return
                        end
                        errored && return

                        if verbose > 0 || rts.anynonpass
                            clear_line()
                            Testset.print_test_results(
                                rts, format;
                                depth = Int(!rts.overall & isindented(verbose, overall, many)),
                                bold = rts.overall | !many,
                                hasbroken=hasbroken
                            )
                        end
                        if rts.anynonpass
                            print_overall()
                            println()
                            Testset.print_test_errors(rts)
                            errored = true
                            allpass = false
                            ndone = length(tests)
                        end
                        nprinted += 1
                        if rts.exception !== nothing
                            exception[] = rts.exception
                        end
                        if nprocs() == 1
                            put!(computechan, nothing)
                        end
                    end
                end
            end # printer task

            ndone = 0

            if overall || !many
                # + if overall, we print the module as a header, to know where the currently
                #   printed testsets belong
                # + if !many, we won't print the overall afterwads, which would be redundant
                #   with the only one printed top-level testset
                ntests += 1
                put!(outchan, module_ts) # printer task will take care of feeding computechan
            else
                @async put!(computechan, nothing)
            end

            @sync for wrkr in workers()
                @async begin
                    if nprocs() == 1
                        take!(computechan)
                    end
                    file = nothing
                    idx = 0
                    while ndone < length(tests) && !interrupted[]
                        ndone += 1
                        if !@isdefined(groups)
                            ts = tests[ndone]
                        else
                            if file === nothing
                                if isempty(groups)
                                    idx = 1
                                else
                                    idx, file = popfirst!(groups)
                                end
                            end
                            idx = findnext(todo, idx) # when a wrkr has file==nothing, it might steal an item from group of another
                                                      # worker, so in any case we must search for a non-done item
                            ts = tests[idx]
                            todo[idx] = false
                            if idx == length(tests) || file === nothing ||
                                    tests[idx+1].source.file != file
                                file = nothing
                            else
                                idx += 1
                            end
                        end

                        if previewchan !== nothing
                            desc = ts.desc
                            desc = desc isa String ?
                                desc :
                                join(replace(desc.args) do part
                                         part isa String ?
                                             part :
                                             "?"
                                     end)
                            desc = "\0" * desc
                            # even when nworkers() >= 2, we inform the previewer that
                            # computation is gonna happen, so the wheel can start spinning
                            put!(previewchan, desc)
                        end

                        chan = (out=outchan, compute=computechan, preview=previewchan)
                        resp = remotecall_fetch(wrkr, mod, ts, regex, chan
                                             ) do mod, ts, regex, chan
                                mts = make_ts(ts, regex, format.stats, chan)
                                Core.eval(mod, mts)
                            end
                        if resp isa Vector
                            ntests += length(resp)
                            append!(module_ts.results, resp)
                        else
                            ntests += 1
                            push!(module_ts.results, resp)
                        end

                    end
                end # wrkr: @async
            end # @sync for wrkr...

            # TODO: maybe put the following stuff in a finally clause where we schedule worker
            # (as part of the mechanism to handle exceptions vs interrupt[])
            put!(outchan, nothing)
            previewchan !== nothing &&
                put!(previewchan, nothing)
            wait(printer)
        end # worker = @task begin ...

        try
            if previewchan !== nothing # then nthreads() > 1
                # we try to keep thread #1 free of heavy work, so that the previewer stays
                # responsive
                tid = rand(2:nthreads())
                thread_pin(worker, UInt16(tid))
            else
                schedule(worker)
            end

            wait(worker)
            previewer !== nothing &&
                wait(previewer)

        catch ex
            interrupted[] = true
            ex isa InterruptException ||
                rethrow()
        end

        @assert interrupted[] || !allpass || nprinted == ntests
        if isassigned(exception)
            throw(exception[])
        end

        nmodules > 1 && verbose > 0 &&
            println()
    end
    nmodules > 1 && !dry &&
        Testset.print_test_results(root, format, bold=true, hasbroken=hasbroken)
    nothing
end

# cf. https://github.com/JuliaLang/julia/issues/34267#issuecomment-573507670
function thread_pin(t::Task, tid::UInt16)
    ccall(:jl_set_task_tid, Cvoid, (Any, Cint), t, tid-1)
    schedule(t)
    return t
end

function process_args(args, verbose, shuffle)
    ########## process args
    local pattern
    modules = Module[]
    for arg in args
        if arg isa Union{AbstractString,Regex}
            @isdefined(pattern) && throw(ArgumentError("cannot pass multiple patterns"))
            pattern = arg
        else
            push!(modules, arg)
        end
    end

    ########## process pattern
    regex =
        if !@isdefined(pattern)
            r""
        elseif pattern isa Regex
            pattern
        else
            if VERSION >= v"1.3"
                r""i * pattern
            else
                Regex(pattern, "i")
            end
        end

    ########## process verbose
    if !isinteger(verbose) && !isinf(verbose) || signbit(verbose)
        throw(ArgumentError("`verbose` must be a non-negative integer or `Inf`"))
    end
    if verbose > typemax(Int)
        verbose = typemax(Int) # can't use `max`, which promotes to Float64 with Inf
    end
    verbose = Int(verbose)

    computemodules!(modules, shuffle), regex, verbose
end

function computemodules!(modules::Vector{Module}, shuffle)
    if isempty(modules)
        # TESTED_MODULES is not up-to-date w.r.t. package modules which have
        # precompilation, so we have to also look in Base.loaded_modules
        # TODO: look recursively in "loaded modules" which use ReTest for sub-modules

        # ALSO: TESTED_MODULES might have "duplicate" entries, i.e. modules with the same
        # name, when one overwrites itself by being redefined; in this case,
        # let's just delete older entries:
        seen = Set{String}()
        for idx in reverse(eachindex(TESTED_MODULES))
            str = string(TESTED_MODULES[idx])
            if str in seen
                TESTED_MODULES[idx] = nothing
            else
                push!(seen, str)
            end
        end
        filter!(!=(nothing), TESTED_MODULES)

        append!(modules, Iterators.flatten((values(Base.loaded_modules), TESTED_MODULES)))
        unique!(modules)
        # will automatically skip ReTest and ReTest.ReTestTest
        filter!(m -> isdefined(m, INLINE_TEST[]) && m ∉ (ReTest, ReTestTest),  modules)
    else
        unique!(modules)
    end
    shuffle && shuffle!(modules)
    modules
end

function fetchtests(mod, regex, verbose, overall)
    tests = updatetests!(mod)
    descwidth = 0
    hasbroken = false

    for ts in tests
        run = resolve!(mod, ts, regex, verbose=verbose)
        run || continue
        descwidth = max(descwidth, ts.descwidth)
        hasbroken |= ts.hasbrokenrec
    end

    tests = filter(ts -> ts.run, tests)
    many = length(tests) > 1
    indented = isindented(verbose, overall, many)

    if indented
        descwidth += 2
    end
    descwidth = max(descwidth, textwidth(string(mod)) + indented)
    tests, descwidth, hasbroken
end

isindented(verbose, overall, many) = (verbose > 0) & (overall | !many)

function dryrun(mod::Module, ts::TestsetExpr, rx::Regex, align::Int=0, parentsubj="", )
    ts.run || return
    desc = ts.desc

    if desc isa String
        subject = parentsubj * '/' * desc
        if isfinal(ts)
            occursin(rx, subject) || return
        end
        println(' '^align, desc)
        for tsc in ts.children
            dryrun(mod, tsc, rx, align + 2, subject)
        end
    else
        loopvals = ts.loopvalues
        if loopvals === nothing
            println(' '^align, desc)
            @warn "could not evaluate testset-for iterator, default to inclusion"
            return
        end
        for x in loopvals
            Core.eval(mod, Expr(:(=), ts.loops.args[1], x))
            descx = Core.eval(mod, desc)::String
            # avoid repeating ourselves, transform this iteration into a "begin/end" testset
            beginend = TestsetExpr(ts.source, ts.mod, descx, ts.options, nothing,
                                   ts.parent, ts.children)
            beginend.run = true
            dryrun(mod, beginend, rx, align, parentsubj)
        end
    end
end

## hijack ####################################################################

"""
    ReTest.hijack(source, [modname]; parentmodule::Module=Main)

Given test files defined in `source` using the `Test` package,
try to load them by replacing `Test` with `ReTest`, wrapping them in a module `modname`
defined withing `parentmodule`. If successful, the newly created module `modname` is
returned and `modname.runtests()` should be callable.

If `source::AbstractString`, then it's interpreted as the top level test file
(possibly including other files).
If `source::Module`, then it's interpreted as the name of a package, and the
"test/runtests.jl" file from this package is loaded. In this case, `modname`
defaults to `Symbol(source, :Tests)`.
"""
function hijack end

function hijack(path::AbstractString, modname=nothing; parentmodule::Module=Main)
    if modname === nothing
        modname = :TestMod
        i = 1
        while hasproperty(parentmodule, modname)
            modname = Symbol(:TestMod, i)
            i += 1
        end
    end
    modname = Symbol(modname)

    newmod = @eval parentmodule module $modname
                                    using ReTest # for files which don't have `using Test`
                                    include($substitue_retest!, $path)
                                end
    newmod
end

function hijack(packagemod::Module, modname=nothing; parentmodule::Module=Main)
    path = joinpath(dirname(dirname(pathof(packagemod))), "test", "runtests.jl")
    if modname === nothing
        modname = Symbol(packagemod, :Tests)
    end
    hijack(path, modname, parentmodule=parentmodule)
end

function substitue_retest!(ex)
    if Meta.isexpr(ex, :using)
        for used in ex.args
            if Meta.isexpr(used, :., 1) && used.args[1] == :ReTest
                throw(ArgumentError("ReTest is already used"))
            elseif Meta.isexpr(used, :., 1) && used.args[1] == :Test
                used.args[1] = :ReTest
            end
        end
    elseif Meta.isexpr(ex, :call) && ex.args[1] == :include
        if length(ex.args) > 2
            throw(ArgumentError("cannot handle include with two arguments"))
        else
            insert!(ex.args, 2, substitue_retest!)
        end
    end
    ex
end


module ReTestTest

using ..ReTest
@testset "test Test in sub-module" begin
    @test 1 == 1
end

end # module ReTestTest

@testset "self test" begin
    @assert typeof(@__MODULE__) == Module
    @test 1 != 2
    retest(ReTestTest)
end

end # module ReTest
