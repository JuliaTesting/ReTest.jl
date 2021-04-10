module ReTest

export retest, @testset, not

using Distributed
using Base.Threads: nthreads
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

using InlineTest: @testset, InlineTest, TESTED_MODULES, INLINE_TEST
import InlineTest: retest

# * Pattern (pre)

# pre-declaration for use in testset.jl

abstract type Pattern end
function matches end


# * includes

include("utils.jl")
include("testset.jl")
include("hijack.jl")

using .Testset: Testset, Format


# * Pattern

const PatternX = Union{Pattern, Regex, Integer}

struct And <: Pattern
    xs::AbstractArray{<:PatternX}
end

struct Or <: Pattern
    xs::AbstractArray{<:PatternX}
end

struct Not <: Pattern
    x::PatternX
end

"""
    not(pattern)

Create an object suitable for filtering testsets (in the [`retest`](@ref) function),
which "negates" the meaning of `pattern`: a testset matches `not(pattern)`
if and only if it doesn't match `pattern`.

For example `not("a")` matches any testset whose subject doesn't contain `"a"`,
and `not(1:3)` matches all the testsets but the first three of a module.
"""
not(x) = Not(make_pattern(x))

alwaysmatches(pat::And) = all(alwaysmatches, pat.xs)

alwaysmatches(pat::Or) =
    if pat.xs isa AbstractArray{<:Integer}
        false # special case for huge unit ranges; locally, this optimization seems
              # unnecessary, i.e. alwaysmatches(Or(1:10...0)) is constant time anyway,
              # but on CI, the any(...) below takes tooooo long
    else
        any(alwaysmatches, pat.xs)
    end

alwaysmatches(::Not) = false
alwaysmatches(rx::Regex) = isempty(rx.pattern)
alwaysmatches(id::Integer) = false

matches(pat::And, x, id) = all(p -> matches(p, x, id), pat.xs)

matches(pat::Or, x, id) =
    if pat.xs isa AbstractUnitRange{<:Integer} && minimum(pat.xs) >= 0
        id ∈ pat.xs # this is optimised, i.e. it's not O(n)
    else
        any(p -> matches(p, x, id), pat.xs)
    end

matches(pat::Not, x, id) = !matches(pat.x, x, id)
matches(rx::Regex, x, _) = occursin(rx, x)
matches(rx::Regex, ::Missing, _) = missing
matches(pat::Integer, _, id) = pat >= 0 ? pat == id : pat != -id

make_pattern(x::PatternX) = x

function make_pattern(str::AbstractString)
    neg = false
    if startswith(str, '-')
        str = chop(str, head=1, tail=0)
        if !startswith(str, '-')
            neg = true
        end
    end

    rx = VERSION >= v"1.3" ? r""i * str :
                             Regex(str, "i")
    neg ? not(rx) : rx
end

make_pattern(pat::AbstractArray) = Or(PatternX[make_pattern(p) for p in pat])
# special case for optimizing unit-ranges:
make_pattern(pat::AbstractArray{<:Integer}) = Or(pat)
make_pattern(@nospecialize(pat::Tuple)) = And(PatternX[make_pattern(p) for p in pat])

hasinteger(::Regex) = false
hasinteger(::Integer) = true
hasinteger(pat::Union{And,Or}) = any(hasinteger, pat.xs)
hasinteger(pat::Not) = hasinteger(pat.x)


# * TestsetExpr

Base.@kwdef mutable struct Options
    verbose::Bool = false # annotated verbosity
    transient_verbose::Bool = false # verbosity for next run
end

mutable struct TestsetExpr
    id::Int64 # unique ID per module (64 bits to be on the safe side)
    source::LineNumberNode
    mod::String # enclosing module
    desc::Union{String,Expr}
    options::Options
    # loops: the original loop expression, if any, but where each `x=...` is
    # pulled out into a vector
    loops::Union{Vector{Expr},Nothing}
    parent::Union{TestsetExpr,Nothing}
    children::Vector{TestsetExpr}
    strings::Vector{String}
    # loopvalues & loopiters: when successful in evaluating loop values in resolve!,
    # we "flatten" the nested for loops into a single loop, with loopvalues
    # containing tuples of values, and loopiters the tuples of variables to which the
    # values are assigned
    loopvalues::Union{Nothing,Vector{Any}}
    loopiters::Union{Nothing,Expr}
    hasbroken::Bool
    hasbrokenrec::Bool # recursive hasbroken, transiently
    run::Bool
    descwidth::Int # max width of self and children shown descriptions
    body::Expr

    TestsetExpr(source, mod, desc, options, loops, parent, children=TestsetExpr[]) =
        new(0, source, mod, desc, options, loops, parent, children, String[])
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
    elseif x.head == :call && x.args[1] == :include
        path = x.args[end]
        sourcepath = dirname(string(source.file))
        x.args[end] = path isa AbstractString ?
            joinpath(sourcepath, path) :
            :(joinpath($sourcepath, $path))
        x, false
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

    body = args[end]
    isa(body, Expr) || error("Expected begin/end block or for loop as argument to @testset")
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
        error("Expected begin/end block or for loop as argument to @testset")
    end

    ts = TestsetExpr(source, mod, desc, options, loops, parent)
    ts.body, ts.hasbroken = replace_ts(source, mod, tsbody, ts)
    ts, false # hasbroken counts only "proper" @test_broken, not recursive ones
end


# this function does 3 things by going recursively through nested testsets:
# - update ts.hasbrokenrec to know whether we print the "broken" column
# - compute ts.descwidth, to know the overall alignment of the first vertical bar
#   (only needed when verbose is large enough)
# - the most important: sorting out which testsets must be run
#   (and in the process, precompute descriptions when possible, and IDs)
#
# Concerning the last point, we make the following compromise: as it's probably
# rare that a Regex matches a given testset but not its children
# (as in r"a$" for the subjects "/a" and "/a/b"), and in order to reduce the
# computational load of resolve!, once a testset is found to have to run,
# its children are automatically assumed to have to run; the correct filtering
# will then happen only for final testsets. The drawback is a risk for
# more compilation than necessary, and wasted runtime while executing
# children testsets.
# TODO: implement the alternative to make a real comparison

function resolve!(mod::Module, ts::TestsetExpr, pat::Pattern;
                  verbose::Int, id::Int64, # external calls
                  force::Bool=false, shown::Bool=true, depth::Int=0) # only recursive calls

    strings = empty!(ts.strings)
    desc = ts.desc
    ts.run = force || alwaysmatches(pat)
    ts.loopvalues = nothing # unnecessary ?
    ts.loopiters = nothing
    ts.id = id
    id += 1

    parentstrs = ts.parent === nothing ? [""] : ts.parent.strings
    ts.descwidth = 0
    ts.options.transient_verbose = shown & ((verbose > 1) | ts.options.verbose)

    # TODO: probably no need to eval the descriptions when they won't be shown
    # and ts.run == true

    function giveup()
        ts.run = coalesce(matches(pat, missing, ts.id), true)
        # if matches yields missing, we don't really know at this point, so be
        # conservative and default to true

        if shown
            # set ts.descwidth to a lower bound to reduce misalignment
            ts.descwidth = 2*depth + mapreduce(+, desc.args) do part
                                         if part isa String
                                             textwidth(part)
                                         else
                                             4 # give 4 spaces for unknown string part
                                         end
                                     end
        end
    end

    loops = ts.loops
    if loops === nothing || desc isa String
        # TODO: maybe, for testset-for and !(desc isa String), still try this branch
        # in case the the interpolation can be resolved thanks to a global binding
        # (i.e. the description doesn't depend on loop variables)
        gaveup = false
        if !(desc isa String)
            # TODO: compute desc only when !ts.run (i.e. it wasn't forced) ?
            try
                desc = Core.eval(mod, desc)
            catch
                giveup()
                gaveup = true
            end
        end
        if !gaveup
            if shown
                ts.descwidth = textwidth(desc) + 2*depth
            end
            for str in parentstrs
                ts.run && break
                new = str * '/' * desc
                if matches(pat, new, ts.id)
                    ts.run = true
                else
                    push!(strings, new)
                end
            end
        end
    else # we have a testset-for with description which needs interpolation
        xs = ()
        loopiters = Expr(:tuple, (arg.args[1] for arg in loops)...)

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
            ts.loopiters = loopiters
        catch
            @assert xs == ()
            giveup()
        end
        for x in xs # empty loop if eval above threw
            descx = eval_desc(mod, ts, x)
            if descx === nothing
                giveup()
                break
            end
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
                if matches(pat, new, ts.id)
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
        runc, id = resolve!(mod, tsc, pat, force=ts.run,
                            shown=shown & ts.options.transient_verbose,
                            depth=depth+1, verbose=verbose-1, id=id)
        run |= runc
        ts.descwidth = max(ts.descwidth, tsc.descwidth)
        if tsc.run
            ts.hasbrokenrec |= tsc.hasbrokenrec
        end
    end
    if !run || verbose <= 0
        ts.descwidth = 0
    end
    ts.run = run
    run, id
end

eval_desc(mod, ts, x) =
    if ts.desc isa String
        ts.desc
    else
        try
            Core.eval(mod, quote
                      let $(ts.loopiters) = $x
                          $(ts.desc)
                      end
                      end)::String
        catch
            nothing
        end
    end

# convert a TestsetExpr into an actually runnable testset
function make_ts(ts::TestsetExpr, pat::Pattern, stats, chan)
    ts.run || return nothing

    if isfinal(ts)
        body = ts.body
    else
        body = make_ts(ts.body, pat, stats, chan)
    end

    if ts.loops === nothing
        quote
            @testset $(ts.mod) $(isfinal(ts)) $pat $(ts.id) $(ts.desc) $(ts.options) $stats $chan $body
        end
    else
        c = count(x -> x === nothing, (ts.loopvalues, ts.loopiters))
        @assert c == 0 || c == 2
        if c == 0
            loops = [Expr(:(=), ts.loopiters, ts.loopvalues)]
        else
            loops = ts.loops
        end
        quote
            @testset $(ts.mod) $(isfinal(ts)) $pat $(ts.id) $(ts.desc) $(ts.options) $stats $chan $loops $body
        end
    end
end

make_ts(x, pat, _, _) = x
make_ts(ex::Expr, pat, stats, chan) =
    Expr(ex.head, map(x -> make_ts(x, pat, stats, chan), ex.args)...)

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
            if !(revise_pkgid() in keys(Base.loaded_modules))
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

revise_pkgid() = Base.PkgId(Base.UUID("295af30f-e4ad-537b-8983-00126c2a3abe"), "Revise")

"accepted types as positional arguments of `retest`"
const ArgType = Union{Module,PatternX,AbstractString,AbstractArray,Tuple,
                      Pair{Module,
                           <:Union{PatternX,AbstractString,AbstractArray,Tuple}}}

"""
    retest(mod..., pattern...; dry::Bool=false, stats::Bool=false, verbose::Real=true, [id::Bool],
                               shuffle::Bool=false, recursive::Bool=true)

Run all the tests declared in `@testset` blocks, within modules `mod` if specified,
or within all currently loaded modules otherwise.

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

### Filtering

It's possible to filter run testsets by specifying one or multiple `pattern`s.
A testset is guaranteed to run only if it "matches" all passed patterns (conjunction).
Moreover if a testset is run, its enclosing testset, if any, also has to run
(although not necessarily exhaustively, i.e. other nested testsets
might be filtered out).

A `pattern` can be a string, a `Regex`, an integer or an array.
For a testset to "match" an array, it must match at least one of its elements (disjunction).
Tp match a tuple, it must match all of its elements (conjunction).
To match an integer, its ID must be equal to this integer (cf. the `id` keyword).

A pattern can also be the "negation" of a pattern, via the [`not`](@ref) function,
which allows to exclude testsets from being run.
As a special case, the negation of an integer can be expressed as its arithmetic
negation, e.g. `not(3)` is equivalent to `-3`.


### `Regex` filtering

The "subject" of a testset is the concatenation of the subject of its parent `@testset`,
if any, with `"/\$description"` where `description` is the testset's description.
For example:
```julia
@testset "a" begin # subject == "/a"
    @testset "b" begin # subject is "/a/b"
    end
    @testset "c\$i" for i=1:2 # subjects are "/a/c1" & "/a/c2"
    end
end
```

When `pattern` isa a `Regex`, a testset is guaranteed to run only when its subject
 matches `pattern`.
Moreover, even if a testset matches (e.g. "/a" above with `pattern == r"a\$"`),
its nested testsets might be filtered out if they don't also match
(e.g. "a/b" doesn't match `pattern`).

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
Note that the `recursive` keyword does _not_ apply to modules from a pair.


!!! note
    this function executes each (top-level) `@testset` block using `eval` *within* the
    module in which it was written (e.g. `mod`, when specified).
"""
function retest(@nospecialize(args::ArgType...);
                dry::Bool=false,
                stats::Bool=false,
                shuffle::Bool=false,
                group::Bool=true,
                verbose::Real=true, # should be @nospecialize, but not supported on old Julia
                recursive::Bool=true,
                id=nothing,
                )

    implicitmodules, modules, verbose = process_args(args, verbose, shuffle, recursive)
    overall = length(modules) > 1
    root = Testset.ReTestSet("", "Overall", overall=true)

    maxidw = Ref{Int}(0) # visual width for showing IDs (Ref for mutability in hack below)
    tests_descs_hasbrokens = fetchtests.(modules, verbose, overall, Ref(maxidw))
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
        join(stdout,
             string.(first.(getindex.((modules,), emptymods))),
             ", ", " and ")
        println('.')
        return
    end

    id = something(id, dry | any(modules) do (mod, pat)
                                 hasinteger(pat)
                             end)
    maxidw[] = id ? maxidw[] : 0

    for imod in eachindex(modules)
        mod, pat = modules[imod]
        tests = alltests[imod]
        isempty(tests) && continue

        shuffle &&
            shuffle!(tests)

        if dry
            showmod = overall || implicitmodules
            if showmod
                imod > 1 && verbose > 0 &&
                    println()
                printstyled(mod, '\n', bold=true)
            end
            foreach(ts -> dryrun(mod, ts, pat, id ? 0 : showmod*2,
                                 verbose=verbose>0, maxidw = id ? maxidw[] : 0), tests)
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

        module_ts = Testset.ReTestSet("", string(mod) * ':', overall=true)
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
                                hasbroken=hasbroken,
                                maxidw=maxidw[]
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
            if previewchan !== nothing && nthreads() > 1
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

function process_args(@nospecialize(args), verbose, shuffle, recursive)
    ########## process args
    baremods = Module[] # list of standalone modules (not in a pair)
    patterns = PatternX[]       # list of standalone patterns
    modpats = []        # pairs, plus "pair-ized" standalone modules

    for arg in args
        if arg isa Module
            arg in baremods && continue # we uniquify only in the simple cases
            push!(modpats, arg => And(patterns))
            push!(baremods, arg)
        elseif arg isa Pair{Module}
            push!(modpats,
                  first(arg) => And(PatternX[make_pattern(last(arg)), And(patterns)]))
        else
            push!(patterns, make_pattern(arg))
        end
    end
    if !allunique(first.(modpats))
        throw(ArgumentError("some modules specified multiple times"))
        # TODO: show name of said modules
        # NOTE: we _could_ merge the specifications for a given module, but it seems better
        # to tell the user about potential error.
        # Also, currently we can't simply run successively both `mod=>pat1` and `mod=>pat2`
        # in a row, because `resolve!` is run for all pairs before running the tests
        # (i.e. it's called before the big loop over modules in `retest`), and the testsets
        # carry some state for a given pattern (of course this would be fixable).
    end

    implicitmodules = isempty(modpats)

    ########## process modules
    if implicitmodules || !isempty(baremods)
        # + we automatically select all loaded modules with ReTest (1st condition), or
        # + we need to look for recursive (2nd condition), which applies only to baremods
        lmods = length(baremods)
        computemodules!(baremods, recursive)
        newmods = splice!(baremods, lmods+1:length(baremods))
        # remove from newmods those already present in modpats
        filter!(newmods) do mod
            all(modpats) do (m, p)
                m != mod
            end
        end
        append!(modpats, newmods .=> Ref(And(patterns)))
    end
    shuffle && shuffle!(modpats)

    ########## process verbose
    if !isinteger(verbose) && !isinf(verbose) || signbit(verbose)
        throw(ArgumentError("`verbose` must be a non-negative integer or `Inf`"))
    end
    if verbose > typemax(Int)
        verbose = typemax(Int) # can't use `max`, which promotes to Float64 with Inf
    end
    verbose = Int(verbose)

    implicitmodules, modpats, verbose
end

function computemodules!(modules::Vector{Module}, recursive)
    if isempty(modules) || recursive # update TESTED_MODULES
        # TESTED_MODULES might have "duplicate" entries, i.e. modules with the same
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
        filter!(x -> x !== nothing, TESTED_MODULES)

        # TESTED_MODULES is not up-to-date w.r.t. package modules which have
        # precompilation, so we have to also look in Base.loaded_modules
        for mod in values(Base.loaded_modules)
            # exclude modules from Main, which presumably already had a chance to get
            # registered in TESTED_MODULES at runtime
            mod ∈ (ReTest, Main, Base) && continue # TODO: should exclude stdlibs too
            str = string(mod)
            if str ∉ seen
                push!(seen, str) # probably unnecessary, if str are all unique in this loop
                for sub in recsubmodules(mod)
                    if isdefined(sub, INLINE_TEST[]) && sub ∉ TESTED_MODULES
                        # sub might be a submodule of a Main-like module mod (e.g. via a
                        # REPL "contextual module"), in which case it already got registered
                        push!(TESTED_MODULES, sub)
                    end
                end
            end
        end

        @assert all(m -> m isa Module, TESTED_MODULES)
        @assert allunique(TESTED_MODULES)
    end
    if isempty(modules)
        # recursive doesn't change anything here
        append!(modules, TESTED_MODULES) # copy! not working on Julia 1.0
    else
        @assert allunique(modules)
        if recursive
            for mod in TESTED_MODULES
                mod ∈ modules && continue
                par = mod
                while true
                    newpar = parentmodule(par)
                    newpar == par && break
                    par = newpar
                    if par ∈ modules
                        push!(modules, mod)
                        break
                    end
                end
            end
        end
    end
    modules
end

function fetchtests((mod, pat), verbose, overall, maxidw)
    tests = updatetests!(mod)
    descwidth = 0
    hasbroken = false

    id = 1
    for ts in tests
        run, id = resolve!(mod, ts, pat, verbose=verbose, id=id)
        run || continue
        descwidth = max(descwidth, ts.descwidth)
        hasbroken |= ts.hasbrokenrec
    end
    maxidw[] = max(maxidw[], ndigits(id-1))

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

function dryrun(mod::Module, ts::TestsetExpr, pat::Pattern, align::Int=0, parentsubj=""
                ; evaldesc=true, repeated=nothing, verbose, maxidw::Int)
    ts.run && verbose || return
    desc = ts.desc

    if ts.loops === nothing
        if evaldesc && !(desc isa String)
            try
                desc = Core.eval(mod, desc)
            catch
            end
        end

        subject = nothing
        if parentsubj isa String && desc isa String
            subject = parentsubj * '/' * desc
            if isfinal(ts)
                matches(pat, subject, ts.id) || return
            end
        end

        if maxidw > 0 # width (ndigits) of max id; <= 0 means ids not printed
            printstyled(lpad(ts.id, maxidw), "| ", color = :light_black, bold=true)
        end
        printstyled(' '^align, desc, color = desc isa String ? :normal : Base.warn_color())

        if repeated !== nothing
            printstyled(" (repeated",
                        repeated == -1 ? ")" : " $repeated times)", '\n',
                        color=:light_black)
        else
            println()
        end
        for tsc in ts.children
            dryrun(mod, tsc, pat, align + 2, subject, verbose=ts.options.transient_verbose,
                   maxidw=maxidw)
        end
    else
        function dryrun_beginend(descx, repeated=nothing)
            # avoid repeating ourselves, transform this iteration into a "begin/end" testset
            beginend = TestsetExpr(ts.source, ts.mod, descx, ts.options, nothing,
                                   ts.parent, ts.children)
            beginend.run = true
            beginend.id = ts.id
            dryrun(mod, beginend, pat, align, parentsubj; evaldesc=false,
                   repeated=repeated, verbose=verbose, maxidw=maxidw)
        end

        loopvalues = ts.loopvalues
        if loopvalues === nothing
            # ts.desc is probably a String (cf. resolve!); if so, don't print repeated
            # identitical lines (caveat: if subjects of children would change randomly)
            # but still try simply to evaluate the length of the iterator
            repeated = -1
            if ts.desc isa String
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
            dryrun_beginend(ts.desc, repeated)
        else
            for (i, x) in enumerate(loopvalues)
                descx = eval_desc(mod, ts, x)
                if descx === nothing
                    @assert i == 1
                    return dryrun_beginend(ts.desc, length(loopvalues))
                end
                dryrun_beginend(descx === nothing ? ts.desc : descx)
            end
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
