module ReTest

export retest, @testset, @testset_macro, not, interpolated, reachable, depth, pass, fail, iter

using Distributed
using Base.Threads: nthreads
import Base: ==
using Random: shuffle!, randstring

# from Test:
export Test,
    @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated,
    @inferred,
    detect_ambiguities, detect_unbound_args,
    GenericString, GenericSet, GenericDict, GenericArray, GenericOrder

using Test: Test,
    @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated,
    @inferred,
    detect_ambiguities, detect_unbound_args,
    GenericString, GenericSet, GenericDict, GenericArray

if isdefined(Test, :GenericOrder)
    using Test: GenericOrder
end

using InlineTest: @testset, InlineTest, TESTED_MODULES, TESTSET_MACROS, INLINE_TEST
import InlineTest: retest, @testset_macro

# * Pattern (pre)

# pre-declaration for use in testset.jl

abstract type Pattern end
function matches end
function setresult! end


# * includes

include("utils.jl")
include("marks.jl")
include("testset.jl")
include("hijack.jl")
include("watch.jl")
include("patterns.jl")

using .Testset: Testset, Format, print_id


# * TestsetExpr

Base.@kwdef mutable struct Options
    verbose::Bool = false # annotated verbosity
    transient_verbose::Bool = false # verbosity for next run
    static_include::Bool = false # whether to execute include at `replace_ts` time
    include_functions::Vector{Symbol} = [:include] # functions to treat like include
end

mutable struct TestsetExpr
    id::Int64 # unique ID per module (64 bits to be on the safe side)
    source::LineNumberNode
    mod::Module # enclosing module
    desc::Union{String,Expr}
    options::Options
    marks::Marks
    # loops: the original loop expression, if any, but where each `x=...` is
    # pulled out into a vector
    loops::Maybe{Vector{Expr}}
    parent::Maybe{TestsetExpr}
    children::Vector{TestsetExpr}
    strings::Vector{Union{String,Missing}}
    # loopvalues & loopiters: when successful in evaluating loop values in resolve!,
    # we "flatten" the nested for loops into a single loop, with loopvalues
    # containing tuples of values, and loopiters the tuples of variables to which the
    # values are assigned
    loopvalues::Union{Nothing,Vector{Any}}
    loopiters::Maybe{Expr}
    hasbroken::Bool
    hasbrokenrec::Bool # recursive hasbroken, transiently
    iter::Union{Int,UnitRange{Int}} # transient loop iteration counter
    run::Bool
    descwidth::Int # max width of self and children shown descriptions
    body::Expr

    TestsetExpr(source, mod, desc, options, marks, loops, parent, children=TestsetExpr[]) =
        new(0, source, mod, desc, options, marks, loops, parent, children, String[])
end

isfor(ts::TestsetExpr) = ts.loops !== nothing

# ts has no children, or at least none which will run this time
isfinal(ts::TestsetExpr) = all(tsc -> !tsc.run, ts.children)

function tsdepth(ts::Union{TestsetExpr,Testset.ReTestSet})
    d = 1
    while ts.parent !== nothing
        d += 1
        ts = ts.parent
    end
    d
end


struct _Invalid
    global const invalid = _Invalid.instance
end

function extract_testsets(dest)
    function extractor(x)
        if Meta.isexpr(x, :macrocall) && x.args[1] == Symbol("@testset")
            push!(dest, x)
            nothing # @testset move out of the evaluated file
        else
            x
        end
    end
end

# replace unqualified `@testset` by TestsetExpr
function replace_ts(source, mod, x::Expr, parent; static_include::Bool,
                    include_functions::Vector{Symbol})
    if x.head === :macrocall
        name = x.args[1]
        if name === Symbol("@testset")
            @assert x.args[2] isa LineNumberNode
            ts, hasbroken = parse_ts(x.args[2], mod, Tuple(x.args[3:end]), parent;
                                     include_functions=include_functions,
                                     static_include=static_include)
            ts !== invalid && parent !== nothing && push!(parent.children, ts)
            ts, false # hasbroken counts only "proper" @test_broken, not recursive ones
        elseif name === Symbol("@test_broken")
            x, true
        elseif name !== Symbol("@test") && name ∈ TESTSET_MACROS
            # `@test` is generally called a lot, so it's probably worth it to skip
            # the containment test in this case
            x = macroexpand(mod, x, recursive=false)
            replace_ts(source, mod, x, parent; static_include=static_include,
                       include_functions=include_functions)
        else
            @goto default
        end
    elseif x.head == :call && x.args[1] ∈ include_functions
        path = x.args[end]
        sourcepath = dirname(string(source.file))
        x.args[end] = path isa AbstractString ?
                          joinpath(sourcepath, path) :
                          :(joinpath($sourcepath, $path))
        if static_include
            news = InlineTest.get_tests(mod).news
            newslen = length(news)
            try
                Core.eval(mod, x)
            catch
                @warn "could not statically include at $source"
                return x, false
            end
            newstmp = news[newslen+1:end]
            resize!(news, newslen)
            if !isempty(newstmp)
                # we unfortunately re-wrap ts expressions in a `@testset ...` expression :(
                included_ts = Expr(:block,
                                   (Expr(:macrocall, Symbol("@testset"),
                                         # NOTE: tsi.source has no effect here, it will be
                                         # overwritten by source in the replace_ts call
                                         # below; it's currently not very important
                                         tsi.source, tsi.ts...)
                                    for tsi in newstmp)...)
                replace_ts(source, mod, included_ts, parent;
                           static_include=static_include, include_functions=include_functions)
            else
                nothing, false
            end
        else
            x, false
        end
    else @label default
        body_br = map(z -> replace_ts(source, mod, z, parent; static_include=static_include,
                                      include_functions=include_functions),
                      x.args)
        filter!(x -> first(x) !== invalid, body_br)
        Expr(x.head, first.(body_br)...), any(last.(body_br))
    end
end

replace_ts(source, mod, x, _1; static_include::Bool,
           include_functions) = x, false

# create a TestsetExpr from @testset's args
function parse_ts(source::LineNumberNode, mod::Module, args::Tuple, parent=nothing;
                  static_include::Bool=false, include_functions::Vector{Symbol}=[:include])

    function tserror(msg)
        @error msg _file=String(source.file) _line=source.line _module=mod
        invalid, false
    end

    isempty(args) &&
        return tserror("expected begin/end block or for loop as argument to @testset")

    local desc
    options = Options(include_functions=include_functions)
    marks = Marks()
    if parent !== nothing
        append!(marks.hard, parent.marks.hard) # copy! not available in Julia 1.0
        options.static_include = parent.options.static_include
        # if static_include was set in parent, it should have been forwarded also
        # through the parse_ts/replace_ts call chains:
        @assert static_include == parent.options.static_include
        @assert include_functions === parent.options.include_functions
    end
    for arg in args[1:end-1]
        if arg isa String || Meta.isexpr(arg, :string)
            desc = arg
        elseif arg isa QuoteNode && arg.value isa Symbol
            # TODO: support non-literal symbols?
            push!(marks.hard, arg.value)
        elseif Meta.isexpr(arg, :(=))
            optname = arg.args[1]
            optname in fieldnames(Options) ||
                return tserror("unsupported @testset option: $optname")
            if optname == :include_functions
                @assert Meta.isexpr(arg.args[2], :vect)
                if parent !== nothing
                    options.include_functions = Symbol[] # make it non-shared
                end
                for ifn in arg.args[2].args
                    @assert ifn isa QuoteNode
                    push!(options.include_functions, ifn.value)
                end
            else
                # TODO: make that work with non-literals:
                setfield!(options, optname, arg.args[2])
            end
        else
            return tserror("unsupported @testset")
        end
    end

    body = args[end]
    isa(body, Expr) ||
        return tserror("expected begin/end block or for loop as argument to @testset")
    if body.head === :for
        tsbody = body.args[2]
        loops = body.args[1]
        if loops.head == :(=)
            loops = Expr[loops]
        else
            @assert loops.head == :block
            @assert all(arg -> Meta.isexpr(arg, :(=)), loops.args)
            loops = loops.args
        end
        if !@isdefined(desc)
            v = loops[1].args[1]
            desc = Expr(:string, "anonym $(randstring('0':'9')): $v = ", v)
            for l = loops[2:end]
                v = l.args[1]
                push!(desc.args, ", $v = ", v)
            end
        end
    elseif body.head === :block
        loops = nothing
        tsbody = body
        if !@isdefined(desc)
            desc = "anonym $(randstring('0':'9'))"
        end
    else
        return tserror("expected begin/end block or for loop as argument to @testset")
    end

    ts = TestsetExpr(source, mod, desc, options, marks, loops, parent)
    ts.body, ts.hasbroken = replace_ts(source, mod, tsbody, ts;
                                       static_include=options.static_include,
                                       include_functions=options.include_functions)
    ts, false # hasbroken counts only "proper" @test_broken, not recursive ones
end


# this function does 3 things by going recursively through nested testsets:
# - update ts.hasbrokenrec to know whether we print the "broken" column
# - compute ts.descwidth, to know the overall alignment of the first vertical bar
#   (only needed when verbose is large enough)
# - the most important: sorting out which testsets must be run
#   (and in the process, precompute descriptions when possible, and IDs)
#
# Concerning the last point, we have the following alternatives with
# a different compromise, depending on the value of `strict`:
#
# false) as it's probably rare that a Regex matches a given testset but not its
# children (as in r"a$" for the subjects "/a" and "/a/b"), and in order to reduce
# the computational load of resolve!, once a testset is found to have to run,
# its children are automatically assumed to have to run; the correct filtering
# will then happen only for final testsets. The drawback is a risk for
# more compilation than necessary, and wasted runtime while executing
# children testsets.
#
# true) a testset found to have to run doesn't force its children to run.
# The drawback is more exhaustive tree walking and more string churn.

function resolve!(mod::Module, ts::TestsetExpr, pat::Pattern;
                  # external calls
                  verbose::Int, id::Int64, strict::Bool, static::Maybe{Bool},
                  warned::Ref{Bool},
                   # only recursive calls
                  force::Bool=false, shown::Bool=true, depth::Int=0)

    strings = empty!(ts.strings)
    desc = ts.desc
    ts.loopvalues = nothing # unnecessary ?
    loopiters = ts.loopiters =
        if ts.loops === nothing
            nothing
        else
            Expr(:tuple, (arg.args[1] for arg in ts.loops)...)
        end

    if 0 != ts.id != id && !warned[] && has(pat, Integer)
        # this can happen when nested testsets are added and Revise is active
        @warn "testset IDs have changed since last run"
        warned[] = true
    end
    ts.id = id
    id += 1
    ts.run = force | (static !== false) & alwaysmatches(pat, tsdepth(ts))

    parentstrs = ts.parent === nothing ? [""] : ts.parent.strings
    ts.descwidth = 0
    ts.options.transient_verbose = shown & ((verbose > 1) | ts.options.verbose)

    # TODO: probably no need to eval the descriptions when they won't be shown
    # and ts.run == true

    descwidth(desc) =
        if desc !== missing
            textwidth(desc) + 2*depth
        else
            # set width to a lower bound to reduce misalignment
            2*depth + max(6, # give at least 6 spaces for the common case of a unique part
                          mapreduce(+, ts.desc.args) do part
                              if part isa String
                                  textwidth(part)
                              else
                                  4 # give 4 spaces for unknown string part
                              end
                          end)
        end

    function decide(subj)
        m = matches(pat, subj, ts)
        # For the curious reader, setting `s = something(static, missing)`, there
        # are few "formulas" to compute the result without `if`, but using only
        # `coalesce, |, &, ==, !=, ===, !==, (a,b) -> a, (a,b) -> b, (a,b) -> !a,
        # (a,b) -> !b`. The shortest formulas involve 5 such
        # functions `fi` and are of the form
        # `f1(f2(s, m), f3(f4(s, m), f5(s, m)))`, there are about a dozen of them
        # (with redundancy because of functions symmetry).
        # All the solutions have `f1 == (===)`, and the 5 simplest involve
        # `(a, b) -> b`, so only 4 fi functions are really needed:
        # - coalesce(s == m, m) === s | m
        # - (coalesce(s, m) == m) === s | m
        # - coalesce(s, m) | !m === m
        # - coalesce(s, m) | (s == m) === m
        # - (coalesce(s, m) == (s | m))  === m
        # Which one is the most understandable?
        # cf. the file "misc/decide_formulas.jl" for the brute-force algorithm
        if static === false
            m === missing
        else
            coalesce(m, static !== true)
        end
    end

    # hasmissing might be set to true if parentstrs has missing, but we don't bother
    function decide_testset!(desc, hasmissing)
        for str in parentstrs
            !strict && ts.run && break
            new = str * "/" * desc
            # string[end] == new can happen with loops when has(pat, Iter) and desc isa String
            hasmissing && new === missing || !isempty(strings) && strings[end] === new ||
                push!(strings, new)
            hasmissing |= new === missing # comes either from desc or str
            ts.run = ts.run || decide(new)
        end
        hasmissing
    end

    loops = ts.loops
    if loops === nothing || desc isa String && !has(pat, Iter)
        # TODO: maybe, for testset-for and !(desc isa String), still try this branch
        # in case the the interpolation can be resolved thanks to a global binding
        # (i.e. the description doesn't depend on loop variables)

        if !(desc isa String)
            # TODO: compute desc only when !ts.run (i.e. it wasn't forced) ?
            try
                desc = Core.eval(mod, desc)::String
            catch
                desc = missing
            end
        end
        if shown
            ts.descwidth = descwidth(desc)
        end
        ts.iter = 1
        decide_testset!(desc, false)

    else # we have a testset-for with description which needs interpolation, or
         # the iterator must be computed to get an iterator counter
        xs = ()

        try
            # we need to evaluate roughly the following:
            # xsgen = Expr(:comprehension, Expr(:generator, loopiters, loops...))
            # but a comprehension expression returns an array, i.e. loop variables
            # can't depend on previous ones; the correct way is therefore to
            # construct nested generators flattened with a :flatten Expr, or to
            # simply construct directly a for-loop as below
            xssym = gensym() # to not risk to shadow a global variable on which
                             # the iteration expression depends
            xsgen = quote
                let $xssym = []
                    $(Expr(:for, Expr(:block, loops...),
                           Expr(:call, Expr(:., :Base, QuoteNode(:push!)),
                                xssym, loopiters)))
                    $xssym
                end
            end
            xs = Core.eval(mod, xsgen)
            @assert xs isa Vector
            ts.loopvalues = xs
        catch
            @assert xs == ()
            ts.descwidth = shown ? descwidth(missing) : 0
            ts.iter = typemax(Int)
            ts.run = ts.run || decide(missing)
            push!(strings, missing)
        end
        hasmissing = false
        for (iter, x) in enumerate(xs) # empty loop if eval above threw
            descx = eval_desc(mod, ts, x)
            ts.iter = iter
            if shown
                ts.descwidth = max(ts.descwidth, descwidth(descx))
            end
            if ts.run && (!strict || ts.desc isa String)
                if ts.desc isa String || !shown # no need to compute subsequent descx to update ts.descwidth
                    iter = length(xs)
                    break
                else
                    continue
                end
            end
            hasmissing = decide_testset!(descx, hasmissing)
        end
    end

    run = ts.run
    ts.hasbrokenrec = ts.hasbroken
    ts.iter = 1:ts.iter # for children, when reachable is used, set the possible range

    for tsc in ts.children
        runc, id = resolve!(mod, tsc, pat, force = !strict && ts.run,
                            shown=shown & ts.options.transient_verbose, static=static,
                            depth=depth+1, verbose=verbose-1, id=id, strict=strict,
                            warned=warned)
        run |= runc
        ts.descwidth = max(ts.descwidth, tsc.descwidth)
        if tsc.run
            ts.hasbrokenrec |= tsc.hasbrokenrec
        end
    end

    if !run || !shown
        ts.descwidth = 0
    end
    ts.run = run
    run, id
end

eval_desc(mod, ts, x; stack=false) = # stack => x == iterstack in dryrun
    if ts.desc isa String
        ts.desc
    else
        try
            Core.eval(mod,
                      if stack
                          Expr(:let, x, ts.desc)
                      else
                          quote
                              let $(ts.loopiters) = $x
                                  $(ts.desc)
                              end
                          end
                      end)::String
        catch
            missing
        end
    end

# convert a TestsetExpr into an actually runnable testset
function make_ts(ts::TestsetExpr, pat::Pattern, stats, chan)
    ts.run || return nothing

    if isempty(ts.children) # not isfinal(ts), so that children which don't run
                            # are removed from the AST
        body = ts.body
    else
        body = make_ts(ts.body, pat, stats, chan)
    end

    if !isfor(ts)
        quote
            @testset $(ts.mod) $(isfinal(ts)) $pat $(ts.id) $(ts.desc) $(ts.options) #=
            =# $(ts.marks) $stats $chan $body
        end
    else
        c = count(x -> x === nothing, (ts.loopvalues, ts.loopiters))
        @assert c == 0 || c == 1
        if c == 0
            loops = [Expr(:(=), ts.loopiters, ts.loopvalues)]
        else
            loops = ts.loops
        end
        quote
            @testset $(ts.mod) $(isfinal(ts)) $pat $(ts.id) $(ts.desc) $(ts.options) #=
            =# $(ts.marks) $stats $chan $loops $body
        end
    end
end

make_ts(x, pat, _, _) = x
make_ts(ex::Expr, pat, stats, chan) =
    Expr(ex.head, map(x -> make_ts(x, pat, stats, chan), ex.args)...)

# convert raw tests from InlineTest into TestsetExpr tests, and handle overwriting
function updatetests!(mod::Module, dup::Bool)
    tests, news, map = InlineTest.get_tests(mod)
    # work-around lack of ordered-dict
    # map: we keep only the latest version of a test at a given location,
    #      to be Revise-friendly (just an imperfect heuristic)
    #      unless dup is true; if later on dup is false, we overwrite only
    #      the last version; should we delete all of the versions in this case?
    for (tsargs, source) in news
        ts, hasbroken = parse_ts(source, mod, tsargs)
        ts === invalid && continue
        idx = get!(map, ts.desc, length(tests) + 1)
        if idx == length(tests) + 1
            push!(tests, ts)
        else
            if !dup && !(revise_pkgid() in keys(Base.loaded_modules))
                desc = ts.desc isa String ? string('"', ts.desc, '"') : ts.desc
                source = string(ts.source.file, ':', ts.source.line)
                @warn "duplicate description for @testset, overwriting: $desc at $source"
            end
            if dup
                push!(tests, ts)
                map[ts.desc] = length(tests)
            else
                tests[idx] = ts
            end
        end
    end
    empty!(news)
    tests
end

revise_pkgid() = Base.PkgId(Base.UUID("295af30f-e4ad-537b-8983-00126c2a3abe"), "Revise")

# Accepted types as positional arguments of `retest`
const ArgType = Union{Module,PatternX,AbstractString,AbstractArray,Tuple,Symbol,
                      Pair{Module,
                           <:Union{PatternX,AbstractString,AbstractArray,Tuple}}}

# Holds the seed to set before each testset. This is not thread-safe, but it's
# not normal/intended to call retest() concurrently anyway.
const test_seed = Ref{Any}(false)

const retest_defaults = (
    dry       = false,
    stats     = false,
    shuffle   = false,
    group     = true,
    verbose   = true,
    recursive = true,
    id        = nothing,
    strict    = true,
    dup       = false,
    static    = nothing,
    load      = false,
    seed      = false,
    marks     = true,
    tag       = Symbol[],
    spin      = true,
    clear     = false,
)

def(kw::Symbol) =
    if isdefined(Main, :__retest_defaults__)
        # TODO: test __retest_defaults__
        get(Main.__retest_defaults__, kw, retest_defaults[kw])
    else
        retest_defaults[kw]
    end

"""
    get_previewer_channel(
        spin, stdout, version, nthreads, nprocs
    )::Maybe{RemoteChannel}

Optionally returns a channel for previewing testsets yet to be completed.

# Arguments:
- `spin::Bool`: Whether to show an active 'spinner' along with the description
                of the currently executing testset.
- `stdout::DataType`: Output stream to print to.
- `version::VersionNumber`: Current version of Julia being used.
- `nthreads::Integer`: Number of threads available to ReTest.
- `nprocs::Integer`: Number of processes available to ReTest.
"""
function get_previewer_channel(spin, stdout, version, nthreads, nprocs)
    can_call_thread_pin = nthreads > 1 && version >= v"1.3"
    if spin && stdout isa Base.TTY && (can_call_thread_pin || nprocs > 1)
        RemoteChannel(() -> Channel{Maybe{Tuple{Int64,String}}}(Inf))
        # Needs to be "remote" in the case nprocs() == 2, as then nworkers() ==
        # 1, which means the one remote worker will put descriptions on
        # previewchan (if nworkers() > 1, descriptions are not put because we
        # can't predict the order in which they complete, and then the previewer
        # will not show the descriptions, just the spinning wheel)

        # On VERSION < v"1.3" : we can't call `thread_pin` (see below), and in
        # this case previewing doesn't work well, as the worker and previewer
        # tasks can end up in the same thread, and the previewer is not
        # responsive.

        # channel size: if nworkers() == 1, then 2 would suffice (one for the
        # "compilation step", one for @testset execution step, and then the
        # printer would empty the channel; but for two workers and more, this
        # second step is not done, so the buffer needs a size of at least
        # `nworkers()`
    else
        # Otherwise, the previewing doesn't work well, because the worker task
        # keeps the thread busy and doesn't yield enough for previewing to be
        # useful.
        nothing
    end
end

"""
    take_latest!(previewchan)::Tuple{Int64, Union{String, Nothing}}

Gets the ID and description of the latest testset to preview.

# Arguments:
- `previewchan::Maybe{RemoteChannel}`: Object to get ID/description pairs from.
"""
function take_latest!(previewchan)
    local id_desc
    while isready(previewchan)
        # printer/previewer can't take! it, as we locked
        id_desc = take!(previewchan)
    end
    if @isdefined(id_desc)
        something(id_desc, (Int64(0), nothing))
    else
        (Int64(0), "")
    end
end

# TODO: Clarify purpose of `align_overflow`.
# TODO: Clarify purpose of `maxidw` - maximum indentation width?
"""
    get_previewer(
        previewchan, interrupted, printlock, gotprinted, align_overflow,
        verbose, format, module_header, many, maxidw
    )::Maybe{Task}

Optionally schedules and returns a task for previewing the completion of
testsets for the current module, if previewing is enabled.

# Arguments:
- `previewchan::Maybe{RemoteChannel}`: Object for acquiring the latest testset
                                       to preview.
- `interrupted::Threads.Atomic{Bool}`: Whether or not the previewer was
                                       interrupted during execution.
- `printlock::ReentrantLock`: Lock for printing to the output stream.
- `gotprinted::Bool`: Whether the latest testset was printed.
- `align_overflow::Integer`: How many characters overflow.
- `verbose::Bool`: Whether to preview nested testsets.
- `format::.Testset.Format`: Container for formatting information.
- `module_header::Bool`: Whether to print module header before testsets
                         belonging to that module.
- `many::Bool`: Whether there are multiple testsets, individually or in loops.
- `maxidw::Ref{Int}`: Visual width for showing testset IDs.
"""
function get_previewer(
    previewchan, interrupted, printlock, gotprinted, align_overflow, verbose,
    format, module_header, many, maxidw
)
    previewchan !== nothing || return nothing
    previewer = @async try
        timer = ['|', '/', '-', '\\']
        cursor = 0
        desc = ""
        id = Int64(0)
        finito = false

        while !finito && !interrupted[]
            lock(printlock) do
                newid, newdesc = take_latest!(previewchan)
                if newdesc === nothing
                    finito = true
                    return # no need to sleep before looping
                elseif newdesc != ""
                    desc = newdesc
                    id = newid
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
                    if isindented(verbose, module_header, many)
                        description = "  " * description
                    end
                    cursor += 1

                    # when verbose == 0, we still can print the currently run
                    # testset, but then its description might be larger than
                    # `align`, because it was not taken into account for
                    # computing `align`;
                    # `align_overflow` computes how many characters do overflow,
                    # so that the printer can "erase" them later on;
                    # once we overflow, we don't go back (leftwards) until the
                    # printer prints
                    align_overflow =
                        max(align_overflow, textwidth(description) - align)
                    print('\r')
                    print_id(id, maxidw[])
                    printstyled(rpad("$description", align+align_overflow, " "),
                                ' ',
                                timer[mod1(cursor, end)];
                                style...)
                end
            end
            previewer_refresh_duration_seconds = 0.13
            sleep(previewer_refresh_duration_seconds)
        end
    catch ex
        # TODO: clarify what is the correct thing to do here
        if ex isa InterruptException
            interrupted[] = true
            rethrow()
        else
            # then there is probably a bug in the previewer code, but it might
            # be fine for the worker/printer to continue?
            rethrow()
        end
    end # previewer task
    previewer
end

"""
    retest(mod..., pattern...;
           dry::Bool=false, stats::Bool=false, verbose::Real=true,
           [id::Bool], shuffle::Bool=false, recursive::Bool=true,
           static::Union{Bool,Nothing}=nothing, dup::Bool=false,
           load::Bool=false, seed::Union{Integer,Bool}=false,
           marks::Bool=true, tag=[], spin::Bool=true)

Run tests declared with [`@testset`](@ref) blocks, within modules `mod` if specified,
or within all currently loaded modules otherwise.
Filtering `pattern`s can be specified to run only a subset of the tests.

### Keywords

* If `dry` is `true`, don't actually run the tests, just print the descriptions
  of the testsets which would (presumably) run.
* If `stats` is `true`, print some time/memory statistics for each testset.
* If specified, `verbose` must be an integer or `Inf` indicating the nesting level
  of testsets whose results must be printed (this is equivalent to adding the
  `verbose=true` annotation to corresponding testsets); the default behavior
  (`true` or `1`) corresponds to printing the result of top-level testsets.
* If `id` is `true`, a unique (per module) integer ID is printed next to each testset,
  which can be used for filtering. The default value of `id` depends on other options.
* If `shuffle` is `true`, shuffle the order in which top-level testsets within
  a given module are run, as well as the list of passed modules.
* If `recursive` is `true`, the tests for all the recursive submodules of
  the passed modules `mod` are also run.
* The `static` keyword controls testsets filtering: if `true`, only testsets
  which are known to match "statically" the passed patterns, i.e. at filtering
  time, are run. See docstring of [`interpolated`](@ref) for more details.
* If `dup` is `true`, multiple toplevel testsets can have the same
  description. If `false`, only the last testset of a "duplicate group" is
  kept. The default is `false` in order to encourage having unique
  descriptions (useful for filtering) but also and mostly to play well with
  `Revise`. This keyword applies only to newly added testsets since the last
  run.
* When `load` is `true`, for each package module `Mod` which is selected, `retest`
  attempts to also select a corresponding `Main.ModTests` module with the
  same pattern specification, unless such module is already explicitly
  passed as an argument. If this test module doesn't already exist,
  `retest` attempts first to include into `Main` the corresponding test file
  "test/ModTests.jl" which is assumed, if it exists, to define
  one or more test modules (typically `ModTests`); these new test modules
  are associated to `Mod` (they inherit its pattern specification as
  above), and are cached and used again on subsequent invocations.
* If `seed` is provided, it is used to seed the global RNG before running
  the tests. As a special case, if `seed === false` (the default), no seeding
  is performed, and if `seed === true`, a seed is chosen randomly.
* When `marks` and `dry` are `true`, "check marks" are printed next to testsets
  which passed or failed in previous runs, as well as labels.
* The `tag` keyword allows to tag a testset with labels, encoded as symbols.
  When `tag` is a list of symbols, tag all matching testsets with these.
  When `tag` is a symbol, tag all matching testsets with it.
  Instead of a symbol `:sym`, it's possible to instead pass `not(:sym)` in
  order to remove the `:sym` label from matching testsets.
  Currently, `tag` has an effect only if `dry` is `true`.
* When `spin` is `true`, the description of the testset being currently executed
  is shown (if there is only one), as well as a "spinner". This is disabled when
  all the available threads/workers are used to run tests (i.e. typically
  `Threads.nthreads()` should be greater than `1` for `spin` to take effect).
  Note also that this feature slows down a bit the execution of tests.

The default values of these keywords can be overriden by defining a dictionary
or named tuple within `Main` called `__retest_defaults__`, whose keys are
symbols. E.g. `__retest_defaults__ = (verbose=Inf, spin=false)`.


### Filtering

It's possible to filter run testsets by specifying one or multiple `pattern`s.
A testset is guaranteed to run only if it "matches" all passed patterns (conjunction).
Even if a testset is run, its nested testsets might not run if they don't match
the patterns.
Moreover if a testset is run, its enclosing testset, if any, also has to run
(although not necessarily exhaustively, i.e. other nested testsets
might be filtered out).

A `pattern` can be a string, a `Regex`, an integer, a symbol, an array or a tuple.
For a testset to "match" an array, it must match at least one of its elements (disjunction).
To match a tuple, it must match all of its elements (conjunction).
To match an integer, its ID must be equal to this integer (cf. the `id` keyword).
To match a symbol, it must be tagged with that symbol (label).

A pattern can also be the "negation" of a pattern, via the [`not`](@ref) function,
which allows to exclude testsets from being run.
As a special case, the negation of an integer can be expressed as its arithmetic
negation, e.g. `not(3)` is equivalent to `-3`.

Patterns can also be created via [`reachable`](@ref), [`interpolated`](@ref) and
[`depth`](@ref).

### `Regex` filtering

The "subject" of a testset is the concatenation of the subject of its parent `@testset`,
if any, with `"/\$description"` where `description` is the testset's description.
For example:
```julia
@testset "a" begin # subject is "/a"
    @testset "b" begin # subject is "/a/b"
    end
    @testset "c\$i" for i=1:2 # subjects are "/a/c1" and "/a/c2"
    end
end
```

When `pattern` is a `Regex`, a testset is guaranteed to run only when its subject
matches `pattern`.
Moreover, even if a testset matches (e.g. `"/a"` above with `pattern == r"a\$"`),
its nested testsets might be filtered out if they don't also match
(e.g. `"a/b"` doesn't match `pattern`).

If a passed `pattern` is a string, then it is wrapped in a `Regex` with the
"case-insensitive" flag, and must match literally the subjects.
This means for example that `"a|b"` will match a subject like `"a|b"` or `"A|B"`,
but not like `"a"` (only in Julia versions >= 1.3; in older versions,
the regex is simply created as `Regex(pattern, "i")`).

As a special case, if a string pattern starts with the `'-'` character,
it's interpreted as the negation of the pattern corresponding to the
string with `'-'` chopped off, e.g. `"-abc"` is equivalent to `not("abc")`.
Unless the string starts with two `'-'` characters, in which case
the first `'-'` is chopped off, e.g. `"--abc"` will match subjects
such as `"123-abc"`. To negate such a pattern, just use `not`,
e.g. `not("--abc")`.

### Per-module patterns

In addition to modules or patterns, positional arguments of `retest` can also be
a pair of the form `mod => pattern`: then `pattern` is used to filter only
testsets from `mod`; if other "standalone" patterns (not attached to a module) are
specified, they also conjunctively apply to `mod`. For example, a call like
`retest(mod1 => 1:3, mod2, "x")` is equivalent to `retest(mod1 => (1:3, "x"), mod2 => "x")`.
If `recursive` is `true`, `pattern` is also applied to all recursive submodules `sub`
of `mod`; if `sub` is also specified as `sub => subpat`, the patterns are merged,
i.e. this is equivalent to specifying `sub => (pattern, subpat)`.

!!! note
    This function executes each (top-level) `@testset` block using `eval` *within* the
    module in which it was written (e.g. `mod`, when specified).
"""
function retest(@nospecialize(args::ArgType...);
                dry::Bool           = def(:dry),
                stats::Bool         = def(:stats),
                shuffle::Bool       = def(:shuffle),
                group::Bool         = def(:group),
                # should be @nospecialize, but not supported on old Julia
                verbose::Real       = def(:verbose),
                recursive::Bool     = def(:recursive),
                id::Maybe{Bool}     = def(:id),
                strict::Bool        = def(:strict),
                dup::Bool           = def(:dup),
                static::Maybe{Bool} = def(:static),
                load::Bool          = def(:load),
                seed::Integer       = def(:seed),
                marks::Bool         = def(:marks),
                tag                 = def(:tag),
                spin::Bool          = def(:spin),
                # clear: clear marks for matching tests, only active if dry=true
                clear::Bool         = def(:clear),
                )

    dry, stats, shuffle, group, verbose, recursive, id, strict, dup, static, marks, spin =
        update_keywords(args, dry, stats, shuffle, group, verbose, recursive, id, strict, dup, static, marks, spin)

    module_header, modules, verbose = process_args(args; verbose=verbose, shuffle=shuffle,
                                                   recursive=recursive, load=load)
    root = Testset.ReTestSet(Main, "Overall", overall=true)

    maxidw = Ref{Int}(0) # visual width for showing IDs (Ref for mutability in hack below)
    tests_descs_hasbrokens = fetchtests.(modules, verbose, module_header, Ref(maxidw);
                                         strict=strict, dup=dup, static=static)
    isempty(tests_descs_hasbrokens) &&
        throw(ArgumentError("no modules using ReTest could be found"))

    alltests = first.(tests_descs_hasbrokens)
    hasbroken = any(last.(tests_descs_hasbrokens))

    emptymods = findall(isempty, alltests)
    nmodules = length(modules) - length(emptymods)
    if nmodules == 0
        plural = length(emptymods) > 1 ? "s" : ""
        print("No matching tests for module$plural ")
        join(stdout,
             string.(first.(getindex.((modules,), emptymods))),
             ", ", " and ")
        println('.')
        return
    end

    id = verbose > 0 && something(id, dry || any(modules) do (mod, pat)
                                                 has(pat, Integer)
                                             end)

    descwidth = maximum(x->x[2], tests_descs_hasbrokens)
    if nmodules > 1 # might be different to overall, if !isempty(emptymods)
                    # this is the condition for printing "Overall" summary
        descwidth = max(descwidth, textwidth(root.description))
    end
    format = Format(stats, descwidth)

    maxidw[] = id ? maxidw[] : 0

    if tag isa Symbol || tag isa Not && tag.x isa Symbol
        tag = Pair[tag => false]
    else
        tag = vec(Pair[t => false for t in tag])
        # values indicate whether a warning was already issued for this tag
    end
    if !dry && !isempty(tag)
        @warn "tag keyword: labels can be added only in dry mode"
    end
    for (t, _) in tag
        label::Symbol = t isa Symbol ? t : t.x
        startswith(String(label), '_') &&
            throw(ArgumentError("tag keyword: labels can't start with an underscore"))
    end

    for imod in eachindex(modules)
        mod, pat = modules[imod]
        tests = alltests[imod]
        isempty(tests) && continue

        shuffle &&
            shuffle!(tests)

        if dry
            if module_header
                imod > 1 && verbose > 0 &&
                    println()
                printstyled(mod, '\n', bold=true)
            end
            if verbose > 0
                foreach(ts -> dryrun(mod, ts, pat, id ? 0 : module_header*2,
                                     maxidw = id ? maxidw[] : 0, marks=marks, tag=tag,
                                     clear=clear),
                        tests)
            end
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

        outchan = RemoteChannel(() -> Channel{Maybe{Testset.ReTestSet}}(0))
        computechan = nprocs() == 1 ?
            Channel{Nothing}(1) : # to not interrupt printer task
            nothing

        ntests = 0
        nprinted = 0
        allpass = true
        exception = Ref{Exception}()
        interrupted = Threads.Atomic{Bool}(false)

        module_ts = Testset.ReTestSet(Main, string(mod) * ':', overall=true)
        push!(root.results, module_ts)

        many = hasmany(tests)

        printlock = ReentrantLock()
        previewchan = get_previewer_channel(
            spin, stdout, VERSION, nthreads(), nprocs()
        )

        gotprinted = false
        align_overflow = 0

        previewer = get_previewer(
            previewchan, interrupted, printlock, gotprinted, align_overflow,
            verbose, format, module_header, many, maxidw
        )

        # TODO: move printer task out of worker?
        worker = @task begin
            printer = @async begin
                errored = false
                finito = false

                print_overall() =
                    if module_summary(verbose, many)
                        # @assert endswith(module_ts.description, ':')
                        if endswith(module_ts.description, ':')
                            module_ts.description = chop(module_ts.description, tail=1)
                        end
                        clear_line()
                        Testset.print_test_results(module_ts, format,
                                                   bold=true, hasbroken=hasbroken,
                                                   maxidw=maxidw[])
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
                            id, desc = take_latest!(previewchan)
                            if desc === nothing
                                # keep `nothing` in so that the previewer knows to terminate
                                put!(previewchan, nothing)
                            end
                        end
                        gotprinted = true

                        if rts === nothing
                            errored && println() # to have empty line after reported error
                            print_overall()
                            finito = true
                            return
                        end
                        errored && return

                        if verbose > 0 || rts.anynonpass
                            clear_line()
                            Testset.print_test_results(
                                rts, format;
                                depth = Int(!rts.overall &
                                            isindented(verbose, module_header, many)),
                                bold = rts.overall | !many,
                                hasbroken=hasbroken,
                                maxidw=maxidw[]
                            )
                        end
                        if rts.anynonpass
                            # TODO: can we print_overall() here,
                            # without having the wrong numbers?
                            # print_overall()
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

            if module_header
                # + if module_header, we print the module as a header, to know where the currently
                #   printed testsets belong
                ntests += 1
                put!(outchan, module_ts) # printer task will take care of feeding computechan
            else
                @async put!(computechan, nothing)
            end

            if seed !== false
                includestr = """
                import ReTest
                ReTest.test_seed[] = $seed
                """

                @everywhere Base.include_string(Main, $includestr)
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
                            put!(previewchan, (ts.id, desc))
                        end

                        chan = (out=outchan, compute=computechan, preview=previewchan)
                        resp = remotecall_fetch(wrkr, mod, ts, pat, chan
                                             ) do mod, ts, pat, chan
                                mts = make_ts(ts, pat, format.stats, chan)
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
            if previewchan !== nothing && nthreads() > 1 && VERSION >= v"1.3"
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
        Testset.print_test_results(root, format, bold=true,
                                   hasbroken=hasbroken, maxidw=maxidw[])
    nothing
end

# cf. https://github.com/JuliaLang/julia/issues/34267#issuecomment-573507670
function thread_pin(t::Task, tid::UInt16)
    ccall(:jl_set_task_tid, Cvoid, (Any, Cint), t, tid-1)
    schedule(t)
    return t
end

# hidden feature, shortcuts for passing kwargs to retest
function update_keywords(@nospecialize(args), dry, stats, shuffle, group, verbose,
                         recursive, id, strict, dup, static, marks, spin)
    for arg in args
        if arg isa Symbol
            sarg = String(arg)
            startswith(sarg, '_') || continue
            for c in sarg[2:end]
                c == 'v' && continue # "verbose" ignored, we care only about the value
                val = islowercase(c)
                c = lowercase(c)
                if isnumeric(c)
                    verbose = parse(Int, c)
                elseif c == 'd'
                    dry = val
                elseif c == 's'
                    stats = val
                elseif c == 'h'
                    shuffle = val
                elseif c == 'g'
                    group = val
                elseif c == 'r'
                    recursive = val
                elseif c == 'i'
                    id = val
                elseif c == 't'
                    strict = val
                elseif c == 'u'
                    dup = val
                elseif c == 'c'
                    static = val
                elseif c == 'm'
                    marks = val
                elseif c == 'p'
                    spin = val
                else
                    error("bad keyword shortcut")
                end
            end
        end
    end
    dry, stats, shuffle, group, verbose, recursive, id, strict, dup, static, marks, spin
end

function process_args(@nospecialize(args);
                      # defaults for keywords are added just for process_args to be more
                      # easily called from test code
                      verbose=def(:verbose), shuffle=def(:shuffle),
                      recursive=def(:recursive), load::Bool=def(:load))
    ########## process args
    patterns = PatternX[] # list of standalone patterns
    modpats = Dict{Module,Any}() # pairs module => pattern
    modules = Module[] # ordered list of keys from modpats
    loaded_modules = Set{Module}(load ? values(Base.loaded_modules) : ())
    toload = Dict{Module,Vector{Module}}() # package => testmodules

    function load_testmod(mod)::Maybe{Vector{Module}}
        mod ∈ loaded_modules || return
        mod in keys(toload) && return
        stestmod = Symbol(mod, :Tests)

        testmods = get(loaded_testmodules, mod, nothing)
        if testmods === nothing && isdefined(Main, stestmod)
            testmod = getfield(Main, stestmod)
            # TODO: test this branch
            if testmod isa Module
                testmods = [testmod]
            else
                @warn "$stestmod exists but is not a module"
            end
        end
        if testmods === nothing
            testmods = ReTest.load(mod, maybe=true)
        end
        if !isempty(testmods)
            toload[mod] = testmods
        end
    end

    # first we initialize modpats with the given patterns for "module-patterns"
    # standalone are added at a later stage, because we want to add them only
    # to "root" modules when recursive=true so that they are not checked multiple
    # times (once for a given module and once for each of its tested parent modules)
    for arg in args
        if arg isa Module
            # if arg was already seen, it already has pattern And() added, so nothing to do
            get!(modpats, arg, And())
            arg ∉ modules && push!(modules, arg)
            load_testmod(arg)
        elseif arg isa Pair{Module}
            mod = first(arg)
            pat = get!(modpats, mod, And())
            push!(pat.xs, make_pattern(last(arg)))
            mod ∉ modules && push!(modules, mod)
            load_testmod(mod)
        elseif arg isa Symbol && startswith(String(arg), '_')
            # ignored, already processed in update_keywords
        else
            push!(patterns, make_pattern(arg))
        end
    end

    # register testmods
    for (mod, testmods) in toload
        for testmod in testmods
            testmod in modules && continue
            @assert !(testmod in keys(modpats))
            modpats[testmod] = deepcopy(modpats[mod])
            # TODO: avoid deepcopy? this is currently added as otherwise `patterns`
            # might be added multiple times at the end of module processing below
            push!(modules, testmod)
        end
    end

    ########## process modules
    @assert allunique(modules)

    implicitmodules = isempty(modpats)
    if implicitmodules || recursive
        update_TESTED_MODULES!(true)
    end
    if implicitmodules
        for mod in @view(TESTED_MODULES[1:end])
            # we iterate only on the current state of TESTED_MODULES (hence the use of a
            # @view), because we don't need to call load_testmod on newly loaded test modules
            load_testmod(mod) # might update TESTED_MODULES with submodules
        end
        update_TESTED_MODULES!(false)
        # TODO: update_TESTED_MODULES!() might need to be called, if a module is
        # replaced by itself within a newly loaded test module? We should add a test
        @assert isempty(modules)
        append!(modules, TESTED_MODULES)
        for mod in modules
            modpats[mod] = And(patterns)
        end
    elseif recursive
        roots = Module[]
        # explore TESTED_MODULES maintaining order to preserve order of appearance of
        # in modules/files
        for mod in unique!([modules; TESTED_MODULES;])
            par = mod
            while true
                newpar = parentmodule(par)
                if newpar == par # no parent in modules was found
                    mod in modules && push!(roots, mod)
                    break
                end
                par = newpar
                if par ∈ modules
                    # we need to attach par's pattern to mod's pattern
                    # it's not a problem if par's pattern is updated later, as the
                    # value in modpats is not changed (but rather mutated in-place),
                    # so mod's pattern will still see the updated pattern of par
                    if mod in modules
                        # modpats[mod]::And must not be set to a new value, as submodules
                        # might already reference it, and the following update must be
                        # visible to them; so we update the .xs field instead
                        push!(modpats[mod].xs, modpats[par])
                    else
                        push!(modules, mod)
                        # patterns for mod and par will always be the same, so no need
                        # to copy; whether par was initially in modules or not, if in a
                        # subsequent iteration (over mod) an intermediate module `inter` is
                        # found (mod << inter << par), we know that `inter` was not
                        # initially in modules, and can therefore also share the same
                        # pattern, i.e. pattern for mod doesn't need to diverge from
                        # that of par
                        modpats[mod] = modpats[par]
                    end
                    break
                end
            end
        end
        for mod in roots
            append!(modpats[mod].xs, patterns)
        end
    else
        for pat in values(modpats)
            append!(pat.xs, patterns)
        end
    end

    # module_header: whether to print module header before testsets belonging to that module
    # we compute module_header before filtering out modules without tests, so that
    # if a parent module with no tests is explicitly passed, which contains a submodule
    # and recursive==true, then the submodule name is printed (and similarly for
    # printing `MyPackageTests` when `MyPackage` is passed and `load==true`)
    module_header = (length(modules) > 1) | implicitmodules

    # remove modules which don't have tests, which can happen when a parent module without
    # tests is passed to retest in order to run tests in its submodules
    filter!(m -> isdefined(m, INLINE_TEST), modules)

    # Remove the precompilation module if we're not precompiling
    if ccall(:jl_generating_output, Cint, ()) == 0
        filter!(m -> nameof(m) !== :_ReTestPrecompileTests, modules)
    end

    shuffle && shuffle!(modules)

    ########## process verbose
    if !isinteger(verbose) && !isinf(verbose) || signbit(verbose)
        throw(ArgumentError("`verbose` must be a non-negative integer or `Inf`"))
    end
    if verbose > typemax(Int)
        verbose = typemax(Int) # can't use `max`, which promotes to Float64 with Inf
    end
    verbose = Int(verbose)

    (module_header=module_header, modules=[mod => modpats[mod] for mod in modules],
     verbose=verbose)
end

function update_TESTED_MODULES!(double_check::Bool=false)
    # TESTED_MODULES might have "duplicate" entries, i.e. modules which were
    # "replaced", when one overwrites itself by being redefined; in this case,
    # let's just delete older entries. We must also take into account the possibility
    # that a module was overwritten, but the new version doesn't have a @testset,
    # in which case there won't be a duplicate, but we must still delete the entry.
    seen = Set{String}()
    for idx in eachindex(TESTED_MODULES)
        if is_replaced(TESTED_MODULES[idx])
            TESTED_MODULES[idx] = nothing
        else
            push!(seen, string(TESTED_MODULES[idx]))
        end
    end
    filter!(x -> x !== nothing, TESTED_MODULES)

    if double_check
        # What is below is obsolete as we now reliably register modules in TESTED_MODULES.
        # We still keep it for a while just to check this assumption.
        # TODO: delete
        #
        # TESTED_MODULES is not up-to-date w.r.t. package modules which have
        # precompilation, so we have to also look in Base.loaded_modules
        for mod in values(Base.loaded_modules)
            # exclude modules from Main, which presumably already had a chance to get
            # registered in TESTED_MODULES at runtime
            mod ∈ (Main, Base) && continue # TODO: should exclude stdlibs too
            str = string(mod)
            if str ∉ seen
                push!(seen, str) # probably unnecessary, if str are all unique in this loop
                for sub in recsubmodules(mod)
                    # new version: just check the assumption
                    nameof(sub) == INLINE_TEST && continue
                    if isdefined(sub, INLINE_TEST)
                        @assert sub in TESTED_MODULES
                    end
                    # old effective version:
                    # if isdefined(sub, INLINE_TEST) && sub ∉ TESTED_MODULES
                    #     # sub might be a submodule of a Main-like module mod (e.g. via a
                    #     # REPL "contextual module"), in which case it already got registered
                    #     push!(TESTED_MODULES, sub)
                    # end
                end
            end
        end
    end

    @assert all(m -> m isa Module, TESTED_MODULES)
    @assert allunique(TESTED_MODULES)
    TESTED_MODULES
end

function fetchtests((mod, pat), verbose, module_header, maxidw; static, strict, dup)
    tests = updatetests!(mod, dup)
    descwidth = 0
    hasbroken = false

    id = Int64(1)
    warned = Ref(false)

    for ts in tests
        run, id = resolve!(mod, ts, pat, verbose=verbose, id=id, strict=strict,
                           static=static, warned=warned)
        run || continue
        descwidth = max(descwidth, ts.descwidth)
        hasbroken |= ts.hasbrokenrec
        maxidw[] = max(maxidw[], ndigits(id-1))
    end

    tests = filter(ts -> ts.run, tests)

    if !isempty(tests)
        many = hasmany(tests)
        indented = isindented(verbose, module_header, many)

        if indented
            descwidth += 2
        end
        if module_header || module_summary(verbose, many)
            descwidth = max(descwidth, textwidth(string(mod)) + module_header) # +1 for ':'
        end
    end
    tests, descwidth, hasbroken
end

isindented(verbose, module_header, many) = (verbose > 0) & module_header
module_summary(verbose, many) = many | iszero(verbose)

# assumes !isempty(tests)
# FIXME: isfor when only one iteration
hasmany(tests) = length(tests) > 1 || isfor(tests[1])

function dryrun(mod::Module, ts::TestsetExpr, pat::Pattern, align::Int=0,
                parentsubj::Union{Missing, String}=""
                # external calls:
                ; maxidw::Int, marks::Bool, tag::Vector, clear::Bool,
                # only recursive calls:
                evaldesc=true, repeated=nothing, show::Bool=true, iterstack=Expr(:block))
    @assert ts.run
    desc = ts.desc

    if !isfor(ts)
        if evaldesc && !(desc isa String)
            try
                desc = Core.eval(mod, desc)
            catch
            end
        end

        subject = desc isa String ? parentsubj * "/" * desc :
                                    missing
        final = isfinal(ts)
        if final || !isempty(tag)
            ismatch = matches(pat, subject, ts)
        end
        if final && false === ismatch
            return false, false, false
        end

        res = pastresult(ts.marks, subject)
        if clear && res !== nothing
            delmark!(ts.marks, subject, res ? _pass : _fail)
            # TODO: should we set res=nothing here ? not doing it might be confusing,
            # but it gives an idea about which marks are being deleted;
        end
        if subject !== missing
            for (ith, (mark, warned)) in enumerate(tag)
                if ismatch
                    if mark isa Symbol
                        addmark!(ts.marks, subject, mark)
                    else
                        justwarned = delmark!(ts.marks, subject, mark.x, warned)
                        if justwarned > warned
                            tag[ith] = mark => justwarned
                        end
                    end
                end
            end
        end

        if show
            print_id(ts.id, maxidw)
            printstyled(' '^align, desc, color = desc isa String ? :normal : Base.warn_color())

            if repeated !== nothing
                printstyled(" (repeated",
                            repeated == -1 ? ")" : " $repeated times)",
                            color=:light_black)
            end

            if marks
                for mark in markiter(ts.marks, subject, true)
                    printstyled(" ", mark, color=:cyan, bold=false)
                end
                if res !== nothing
                    printstyled(res ? " ✔" : " ✘", color = res ? :green : Base.error_color(),
                                bold=true)
                end
            end
        end

        if ts.options.transient_verbose
            @assert show
            println()
            for tsc in ts.children
                tsc.run || continue
                dryrun(mod, tsc, pat, align + 2, subject,
                       maxidw=maxidw, marks=marks, tag=tag, clear=clear, show=true,
                       iterstack=iterstack)
            end
            false, false, false # meaningless unused triple
        elseif marks
            passes, fails, unrun = false, false, false
            if !show
                if res === nothing
                    unrun = true
                elseif res
                    passes = true
                else
                    fails = true
                end
            end
            for tsc in ts.children
                tsc.run || continue
                cp, cf, cu = dryrun(mod, tsc, pat, align + 2, subject,
                                    maxidw=maxidw, marks=marks, tag=tag, clear=clear,
                                    show=false, iterstack=iterstack)
                passes |= cp
                fails |= cf
                unrun |= cu
                passes && fails && unrun && break
            end
            if show
                passes &&
                    printstyled(" ✔", color = :light_black, bold=true)
                fails &&
                    printstyled(" ✘", color = :light_black, bold=true)
                unrun &&
                    # space at the end because of a printing bug (with no trailing space,
                    # only two dots are printed instead of three on this machine)
                    printstyled(" ⋯ ", color = :light_black, bold=true)
                println()
            end
            passes, fails, unrun
        else
            if show
                println()
            end
            false, false, false
        end
    else # isfor(ts)
        function dryrun_beginend(descx; iter, repeated=nothing)
            # avoid repeating ourselves, transform this iteration into a "begin/end" testset
            if descx isa Expr
                @assert descx.head == :string
                descx = Expr(:string, copy(descx.args)...)
                replace!(descx.args) do arg
                    if arg isa String || arg isa Symbol
                        # TODO: unify with same function in previewer, which sets "?"
                        # even for symbols
                        arg
                    else
                        @assert arg isa Expr # just to have a chance to discover other possibilities
                        "?"
                    end
                end
            end
            beginend = TestsetExpr(ts.source, ts.mod, descx, ts.options, ts.marks, nothing,
                                   ts.parent, ts.children)
            beginend.run = true
            beginend.id = ts.id
            beginend.iter = iter
            ts.iter = iter # necessary when reachable is used
            dryrun(mod, beginend, pat, align, parentsubj; evaldesc=false,
                   repeated=repeated, maxidw=maxidw, marks=marks, tag=tag,
                   clear=clear, show=show, iterstack=iterstack)
        end

        loopvalues = ts.loopvalues
        if loopvalues === nothing
            # we check whether we can now evaluate loopvalues via iterstack
            try
                # cf. resolve!
                xssym = gensym()
                xsgen = quote
                    let $xssym = []
                        $(Expr(:for, Expr(:block, ts.loops...),
                               Expr(:call, Expr(:., :Base, QuoteNode(:push!)),
                                    xssym, ts.loopiters)))
                        $xssym
                    end
                end
                loopvalues = Core.eval(mod, Expr(:let, iterstack, xsgen))
                @assert loopvalues isa Vector
            catch
                @assert loopvalues == nothing
            end
        end

        if loopvalues === nothing
            # ts.desc is probably a String (cf. resolve!); if so, don't print repeated
            # identitical lines (caveat: if subjects of children would change randomly)
            # but still try simply to evaluate the length of the iterator
            repeated = -1
            if ts.desc isa String && !has(pat, Iter)
                # when has(pat, Iter), we probably weren't able to eval the forloop-iterator
                # in resolve!, so no need to re-try here; plus, we wouldn't be able in this
                # branch to tell how many iterations are matching, without going thru the
                # dryrun_beginend function in an enumerate'd loop
                local iterlen
                try
                    iterlen = 1
                    for loop in ts.loops
                        iterlen *= Core.eval(mod, :(length($(loop.args[2]))))
                    end
                    repeated = iterlen
                catch
                end
            end
            # if iterlen was computed, then !has(pat, Iter) so the value of the iter
            # keyword below doesn't matter
            dryrun_beginend(ts.desc, repeated=repeated, iter=1:typemax(Int))
        else
            passes, fails, unrun = false, false, false
            for (i, x) in enumerate(loopvalues)
                push!(iterstack.args, Expr(:(=), ts.loopiters, x))
                descx = eval_desc(mod, ts, iterstack, stack=true)
                if descx === missing
                    # we would usually have `i == 1`, but not in some rare cases;
                    # once we find an uninterpolated description, we still assume
                    # for simplicity that all remaining ones will also be uninterpolated,
                    # so we add the "repeated" annotation
                    # (it's certainly not worth it to bother being more precise about
                    # exactly which iterations are uninterpolated)
                    lp, lf, lu = dryrun_beginend(ts.desc, repeated=length(loopvalues)-i+1,
                                                 iter=i:length(loopvalues))
                else
                    lp, lf, lu = dryrun_beginend(descx, iter=i)
                end
                pop!(iterstack.args)
                passes |= lp
                fails |= lf
                unrun |= lu
                descx === missing && break
            end
            passes, fails, unrun
        end
    end
end

# nothing to do with autogenerated `runtests` which is equivalent to `retest`
# this is to make it easier to test ReTest itself
# argument is a list of space-separated tests to run, e.g. "load hijack"
function runtests(tests::String="")
    pkg_pkgid = Base.PkgId(Base.UUID("44cfe95a-1eb2-52ea-b672-e2afdf69b78f"), "Pkg")
    Pkg = get(Base.loaded_modules, pkg_pkgid, nothing)
    Pkg === nothing &&
        error("Pkg is not loaded") # Pkg seems to always be loaded...
    Pkg.test("ReTest", test_args=Vector{String}(split(tests)))
end

include("precompile.jl")

end # module ReTest
