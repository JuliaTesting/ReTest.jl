module ReTest

export retest, @testset

using Distributed
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
    verbose::Bool = false
end

mutable struct TestsetExpr
    source::LineNumberNode
    desc::Union{String,Expr}
    options::Options
    loops::Union{Expr,Nothing}
    parent::Union{TestsetExpr,Nothing}
    children::Vector{TestsetExpr}
    strings::Vector{String}
    loopvalues::Any
    run::Bool
    descwidth::Int # max width of self and children shown descriptions
    body::Expr

    TestsetExpr(source, desc, options, loops, parent, children=TestsetExpr[]) =
        new(source, desc, options, loops, parent, children, String[])
end

isfor(ts::TestsetExpr) = ts.loops !== nothing
isfinal(ts::TestsetExpr) = isempty(ts.children)

# replace unqualified `@testset` by TestsetExpr
function replace_ts(source, x::Expr, parent)
    if x.head === :macrocall && x.args[1] === Symbol("@testset")
        @assert x.args[2] isa LineNumberNode
        ts = parse_ts(source, Tuple(x.args[3:end]), parent)
        parent !== nothing && push!(parent.children, ts)
        ts
    else
        body = map(z -> replace_ts(source, z, parent), x.args)
        Expr(x.head, body...)
    end
end

replace_ts(source, x, _) = x

# create a TestsetExpr from @testset's args
function parse_ts(source, args::Tuple, parent=nothing)
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

    ts = TestsetExpr(source, desc, options, loops, parent)
    ts.body = replace_ts(source, tsbody, ts)
    ts
end

function resolve!(mod::Module, ts::TestsetExpr, rx::Regex;
                  force::Bool=false, shown::Bool=true, depth::Int=0)
    strings = empty!(ts.strings)
    desc = ts.desc
    ts.run = force || isempty(rx.pattern)
    ts.loopvalues = nothing # unnecessary ?

    parentstrs = ts.parent === nothing ? [""] : ts.parent.strings
    ts.descwidth = 0

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

    for tsc in ts.children
        run |= resolve!(mod, tsc, rx, force=ts.run,
                        shown=shown & ts.options.verbose, depth=depth+1)
        ts.descwidth = max(ts.descwidth, tsc.descwidth)
    end
    if !run
        ts.descwidth = 0
    end
    ts.run = run
end

# convert a TestsetExpr into an actually runnable testset
function make_ts(ts::TestsetExpr, rx::Regex, outchan)
    ts.run || return nothing

    if isfinal(ts)
        body = ts.body
    else
        body = make_ts(ts.body, rx, outchan)
    end
    if ts.loops === nothing
        quote
            @testset $(isfinal(ts)) $rx $(ts.desc) $(ts.options) $outchan $body
        end
    else
        loopvals = something(ts.loopvalues, ts.loops.args[2])
        quote
            @testset $(isfinal(ts)) $rx $(ts.desc) $(ts.options) $outchan $(ts.loops.args[1]) $loopvals $body
        end
    end
end

make_ts(x, rx, _) = x
make_ts(ex::Expr, rx, outchan) = Expr(ex.head, map(x -> make_ts(x, rx, outchan), ex.args)...)

# convert raw tests from InlineTest into TestsetExpr tests, and handle overwriting
function updatetests!(mod::Module)
    tests, news, map = InlineTest.get_tests(mod)
    # work-around lack of ordered-dict
    # map: we keep only the latest version of a test at a given location,
    #      to be Revise-friendly (just an imperfect heuristic)
    for (tsargs, source) in news
        ts = parse_ts(source, tsargs)
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
                                       shuffle::Bool=false)

Run all the tests declared in `@testset` blocks, within modules `m` if specified,
or within all currently loaded modules otherwise.
If `dry` is `true`, don't actually run the tests, just print the descriptions
of the testsets which would (presumably) run.
If `stats` is `true`, print some time/memory statistics for each testset.
If `shuffle` is `true`, shuffle the order in which top-level testsets are run.

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
function retest(mod::Module, pattern::Union{AbstractString,Regex} = r"";
                dry::Bool=false,
                stats::Bool=false,
                shuffle::Bool=false,
                group::Bool=true)
    regex = pattern isa Regex ? pattern :
        if VERSION >= v"1.3"
            r""i * pattern
        else
            Regex(pattern, "i")
        end

    tests = updatetests!(mod)

    descwidth = 0
    for ts in tests
        run = resolve!(mod, ts, regex)
        run || continue
        descwidth = max(descwidth, ts.descwidth)
    end

    tests = filter(ts -> ts.run, tests)
    isempty(tests) && return

    shuffle &&
        shuffle!(tests)

    dry &&
        return foreach(ts -> dryrun(mod, ts, regex), tests)

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

    nprinted = 0
    allpass = true
    exception = Ref{Exception}()

    printer = @async begin
        errored = false
        format = Format(stats, descwidth)

        while true
            rts = take!(outchan)
            rts === nothing && break
            errored && continue

            Testset.print_test_results(rts, format)
            if rts.anynonpass
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
        end
    end

    ntests = 0
    ndone = 0
    @sync for wrkr in workers()
        @async begin
            file = nothing
            idx = 0
            while ndone < length(tests)
                ndone += 1
                if !@isdefined(groups)
                    ts = tests[ndone]
                else
                    if file === nothing
                        if isempty(groups)
                            idx = findfirst(todo)
                        else
                            idx, file = popfirst!(groups)
                        end
                    end
                    ts = tests[idx]
                    todo[idx] = false
                    if idx == length(tests) || file === nothing ||
                            tests[idx+1].source.file != file
                        file = nothing
                    else
                        idx += 1
                    end
                end
                resp = try
                    remotecall_fetch(wrkr, mod, ts, regex, outchan) do mod, ts, regex, outchan
                        mts = make_ts(ts, regex, outchan)
                        res = Core.eval(mod, mts)
                        res isa Vector ? length(res) : 1
                    end
                catch e
                    allpass = false
                    ndone = length(tests)
                    isa(e, InterruptException) || rethrow()
                    return
                end
                ntests += resp
            end
        end
    end
    put!(outchan, nothing)
    wait(printer)
    @assert !allpass || nprinted == ntests
    if isassigned(exception)
        throw(exception[])
    end
end

function retest(args::Union{Module,AbstractString,Regex}...; kwargs...)
    pattern = []
    modules = Module[]
    for arg in args
        if arg isa Union{AbstractString,Regex}
            !isempty(pattern) && throw(ArgumentError("cannot pass multiple patterns"))
            push!(pattern, arg)
        else
            push!(modules, arg)
        end
    end
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
    end
    for mod in modules
        retest(mod, pattern...; kwargs...)
    end
end

function dryrun(mod::Module, ts::TestsetExpr, rx::Regex, parentsubj="", align::Int=0)
    ts.run || return
    desc = ts.desc

    if desc isa String
        subject = parentsubj * '/' * desc
        if isfinal(ts)
            occursin(rx, subject) || return
        end
        println(' '^align, desc)
        for tsc in ts.children
            dryrun(mod, tsc, rx, subject, align + 2)
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
            beginend = TestsetExpr(ts.source, descx, ts.options, nothing, ts.parent, ts.children)
            beginend.run = true
            dryrun(mod, beginend, rx, parentsubj, align)
        end
    end
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
