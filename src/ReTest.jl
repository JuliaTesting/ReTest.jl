module ReTest

export retest, @testset, not, interpolated

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
    xs::Vector{PatternX}
end

And() = And(PatternX[])

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

struct Interpolated <: Pattern end

"""
    interpolated

Singleton pattern which matches any testset whose description can be interpolated
"statically", i.e. at filtering time before testset are actually run.
Non-inferrable descriptions include those containing interpolated values
which can't be known until run time.
This pattern has an effect closely related to that of the `static` keyword
of [`retest`](@ref), discussed below, which is probably more generally useful.

# Examples

Given these testset:
```julia
@testset "outer" verbose=true begin
    @test true
    inner = "inner"
    @testset "\$inner" begin
        @test true
    end
end
@testset "other" begin
    @test true
end
```
We get:
```julia
julia> retest("other", dry=true)
Main
1| outer
2|   "\$(inner)"
3| other

julia> retest("other", dry=false)
            Pass
outer   |      1
other   |      1
Main    |      2

julia> retest("other", dry=true, interpolated)
Main
3| other
```

Without `interpolated`, `retest` can't decide at filtering time whether the "inner"
testset will run, so must mark the "outer" testset as having to run. At run
time, "inner" is not run because it doesn't match the pattern, but "outer"
still had to run to determine this. With the `interpolated` pattern, "inner" is
filtered out and `retest` selects only testsets which are statically known to
have to run.

So again, `interpolated` doesn't have the same effect at filtering time (like when
`dry=true`) and at run time.
For example, one can see the list of non-interpolated descriptions as follows with
`dry=true`, but not run them (because everything is interpolated at run time):

```julia
julia> retest(not(interpolated), dry=true)
1| outer
2|   "\$(inner)"

julia> retest(not(interpolated), dry=false)
            Pass
Main:
  outer |      1
```

### `static` keyword

Unlike `interpolated`, the `static` keyword of `retest`, when `true`,
filters out only testsets which can't be proven to have to run at filtering time,
let's call them "undecidable".
It can have sometimes the same effect as when using `interpolated`,
e.g. `retest("other", dry=true, static=true)` and
`retest("other", dry=true, interpolated)` give the same result.

But in some cases we might want to filter out noisy testsets whose
description can't be interpolated, but still include those which are
relevant. For example, assume we want to run testsets `1` and `2`,
while excluding other testsets with uninterpolated descriptions:
```julia
julia> retest(1:2, dry=true, interpolated)
Main
1| outer

julia> retest(1:2, dry=true, static=true)
Main
1| outer
2|   "\$(inner)"
```
The solution with `interpolated` is not what we want, as we specifically
want testset `2` to run. Given the filtering specifications (`1:2` here),
the filtering algorithm can determine that `2` should run even though
its description is unknown at this point.

Given a filtering specification, there are three kind of testsets:
* "undecidable" (see above)
* "match": they are known statically to have to run
* "nomatch": they are known statically to not have to run

The default value of the `static` keyword is `nothing`, which means
to run testsets which are not known with certainty to not match,
i.e. "match" and "undecidable" testsets.
As seen above, when `static == true`, only "match" testsets are run.
When `static == false`, the behavior is the opposite: only "undecidable"
testsets are run.
Of course, other combinations involving "nomatch" testsets can be had
by reversing the filtering pattern via [`not`](@ref).

For example, to get the equivalent to the `not(interpolated)` example above,
but with an effect which persists at run time (`dry = false`),
you can use `static = false` together with the match-all regex pattern `r".*"`,
which will mark the `"inner"` testset as "undecidable"
(the algorithm inspects slightly patterns just to recognize the simple
match-all patterns `""` and `r""`, but won't detect that `r".*"` would
match `"\$(inner)"`):
```julia
julia> retest(r".*", static=false, dry=true)
Main
1| outer
2|   "\$(inner)"

julia> retest(r".*", static=false, dry=false)
            Pass
Main:
  outer |      2
    inner |      1
```

One example of a rare case where a given testset is not in a single of the
above three categories is as follows:

```julia
@testset "a" begin
    x = 2
    @testset "b\$(i==1 ? 1 : x)" for i=1:2
        @testset "c" begin
            # subject is "match" at first iteration and
            # "undecidable" at second iteration
            @test true
        end
    end
end
```

One thing to understand is that the "identity" of a testset is determined
by a given occurrence of the `@testset` macro. In the example above,
for either the patterns "b" or "c", the two inner testsets are both
"match" and "undecidable". In this case, the filtering algorithm
selects a testset to run if at least one iteration would lead to
this decision. Here, if `static=true` the first iteration
would run, and if `static=false` the second iteration would run.
This results in the same selection whatever the value of `static` is.
"""
const interpolated = Interpolated()

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
alwaysmatches(::Interpolated) = false
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
matches(::Interpolated, x::Union{Missing,String}, id) = x !== missing
matches(rx::Regex, x, _) = occursin(rx, x)
matches(rx::Regex, ::Missing, _) = alwaysmatches(rx) | missing
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

    rx =
        if isempty(str)
            r"" # in order to know to match unconditionally
        elseif VERSION >= v"1.3"
            r""i * str
        else
            Regex(str, "i")
        end
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
hasinteger(::Interpolated) = false


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
    strings::Vector{Union{String,Missing}}
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
                  verbose::Int, id::Int64, strict::Bool,
                  static::Union{Bool,Nothing},
                   # only recursive calls
                  force::Bool=false, shown::Bool=true, depth::Int=0)

    strings = empty!(ts.strings)
    desc = ts.desc
    ts.run = force | (static !== false) & alwaysmatches(pat)
    ts.loopvalues = nothing # unnecessary ?
    ts.loopiters = nothing
    if ts.id != 0
        @assert ts.id == id
    end
    ts.id = id
    id += 1

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
            2*depth + mapreduce(+, ts.desc.args) do part
                          if part isa String
                              textwidth(part)
                          else
                              4 # give 4 spaces for unknown string part
                          end
                      end
        end

    function decide(subj)
        m = matches(pat, subj, ts.id)
        # For the curious, setting `s = something(static, missing)`, there are few
        # "formulas" to compute the result without `if`, but using only
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

    loops = ts.loops
    if loops === nothing || desc isa String
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
        hasmissing = false
        for str in parentstrs
            !strict && ts.run && break
            new = str * "/" * desc # TODO: implement *(::Missing, ::Char) in Base ?
            hasmissing |= new === missing # comes either from desc or str
            ts.run = ts.run || decide(new)
            hasmissing && str === missing ||
                push!(strings, new)
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
            ts.descwidth = shown ? descwidth(missing) : 0
            ts.run = ts.run || decide(missing)
        end
        hasmissing = false
        for x in xs # empty loop if eval above threw
            descx = eval_desc(mod, ts, x)
            if shown
                ts.descwidth = max(ts.descwidth, descwidth(descx))
            end
            if !strict && ts.run
                if !shown # no need to compute subsequent descx to update ts.descwidth
                    break
                else
                    continue
                end
            end
            for str in parentstrs
                !strict && ts.run && break
                new = str * "/" * descx
                hasmissing |= new === missing
                ts.run = ts.run || decide(new)
                hasmissing && str === missing ||
                    push!(strings, new)
            end
        end
    end

    run = ts.run
    ts.hasbrokenrec = ts.hasbroken

    for tsc in ts.children
        runc, id = resolve!(mod, tsc, pat, force = !strict && ts.run,
                            shown=shown & ts.options.transient_verbose, static=static,
                            depth=depth+1, verbose=verbose-1, id=id, strict=strict)
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
            missing
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
function updatetests!(mod::Module, dup::Bool)
    tests, news, map = InlineTest.get_tests(mod)
    # work-around lack of ordered-dict
    # map: we keep only the latest version of a test at a given location,
    #      to be Revise-friendly (just an imperfect heuristic)
    #      unless dup is true; if later on dup is false, we overwrite only
    #      the last version; should we delete all of the versions in this case?
    for (tsargs, source) in news
        ts, hasbroken = parse_ts(source, string(mod), tsargs)
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

"accepted types as positional arguments of `retest`"
const ArgType = Union{Module,PatternX,AbstractString,AbstractArray,Tuple,Symbol,
                      Pair{Module,
                           <:Union{PatternX,AbstractString,AbstractArray,Tuple}}}

"""
    retest(mod..., pattern...;
           dry::Bool=false, stats::Bool=false, verbose::Real=true,
           [id::Bool], shuffle::Bool=false, recursive::Bool=true,
           static::Union{Bool,Nothing}=nothing, dup::Bool=false)

Run tests declared with [`@testset`](@ref) blocks, within modules `mod` if specified,
or within all currently loaded modules otherwise.
When no `pattern`s are specified, all the tests are run.

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

### Filtering

It's possible to filter run testsets by specifying one or multiple `pattern`s.
A testset is guaranteed to run only if it "matches" all passed patterns (conjunction).
Even if a testset is run, its nested testsets might not run if they don't match
the patterns.
Moreover if a testset is run, its enclosing testset, if any, also has to run
(although not necessarily exhaustively, i.e. other nested testsets
might be filtered out).

A `pattern` can be a string, a `Regex`, an integer, an array or a tuple.
For a testset to "match" an array, it must match at least one of its elements (disjunction).
To match a tuple, it must match all of its elements (conjunction).
To match an integer, its ID must be equal to this integer (cf. the `id` keyword).

A pattern can also be the "negation" of a pattern, via the [`not`](@ref) function,
which allows to exclude testsets from being run.
As a special case, the negation of an integer can be expressed as its arithmetic
negation, e.g. `not(3)` is equivalent to `-3`.

A pattern can also be the [`interpolated`](@ref) singleton object, cf. its docstring.

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
If `recursive` is `true`, `pattern` is also applied to all recursive submodules `sub`
of `mod`; if `sub` is also specified as `sub => subpat`, the patterns are merged,
i.e. this is equivalent to specifying `sub => (pattern, subpat)`.

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
                strict::Bool=true,
                dup::Bool=false,
                static::Union{Bool,Nothing}=nothing,
                )

    dry, stats, shuffle, group, verbose, recursive, id, strict, dup, static =
        update_keywords(args, dry, stats, shuffle, group, verbose, recursive, id, strict, dup, static)

    implicitmodules, modules, verbose = process_args(args, verbose, shuffle, recursive)
    overall = length(modules) > 1
    root = Testset.ReTestSet("", "Overall", overall=true)

    maxidw = Ref{Int}(0) # visual width for showing IDs (Ref for mutability in hack below)
    tests_descs_hasbrokens = fetchtests.(modules, verbose, overall, Ref(maxidw);
                                         strict=strict, dup=dup, static=static)
    isempty(tests_descs_hasbrokens) &&
        throw(ArgumentError("no modules using ReTest could be found"))

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

# hidden feature, shortcuts for passing kwargs to retest
function update_keywords(@nospecialize(args), dry, stats, shuffle, group, verbose,
                         recursive, id, strict, dup, static)
    for arg in args
        if arg isa Symbol
            for c in string(arg)
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
                else
                    error("bad keyword shortcut")
                end
            end
        end
    end
    dry, stats, shuffle, group, verbose, recursive, id, strict, dup, static
end

function process_args(@nospecialize(args), verbose, shuffle, recursive)
    ########## process args
    patterns = PatternX[] # list of standalone patterns
    modpats = Dict{Module,Any}() # pairs module => pattern
    modules = Module[] # ordered list of keys from modpats

    # first we initialize modpats with the given patterns for "module-patterns"
    # standalone are added at a later stage, because we want to add them only
    # to "root" modules when recursive=true so that they are not checked multiple
    # times (once for a given module and once for each of its tested parent modules)
    for arg in args
        if arg isa Module
            # if arg was already seen, it already has pattern And() added, so nothing to do
            get!(modpats, arg, And())
            arg ∉ modules && push!(modules, arg)
        elseif arg isa Pair{Module}
            mod = first(arg)
            pat = get!(modpats, mod, And())
            push!(pat.xs, make_pattern(last(arg)))
            mod ∉ modules && push!(modules, mod)
        elseif arg isa Symbol
            # ignored, already processed in update_keywords
        else
            push!(patterns, make_pattern(arg))
        end
    end

    ########## process modules
    @assert allunique(modules)

    implicitmodules = isempty(modpats)
    if implicitmodules || recursive
        update_TESTED_MODULES!()
    end
    if implicitmodules
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

    # remove modules which don't have tests, which can happen when a parent module without
    # tests is passed to retest in order to run tests in its submodules
    filter!(m -> isdefined(m, INLINE_TEST), modules)

    shuffle && shuffle!(modules)

    ########## process verbose
    if !isinteger(verbose) && !isinf(verbose) || signbit(verbose)
        throw(ArgumentError("`verbose` must be a non-negative integer or `Inf`"))
    end
    if verbose > typemax(Int)
        verbose = typemax(Int) # can't use `max`, which promotes to Float64 with Inf
    end
    verbose = Int(verbose)

    implicitmodules, [mod => modpats[mod] for mod in modules], verbose
end

function update_TESTED_MODULES!()
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

    # What is below is obsolete as we now reliably register modules in TESTED_MODULES.
    # We still keep it for a while just to check this assumption.
    # TODO: delete
    #
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

    @assert all(m -> m isa Module, TESTED_MODULES)
    @assert allunique(TESTED_MODULES)
    filter!(m -> m ∉ (ReTest, ReTest.ReTestTest), TESTED_MODULES)
end

function fetchtests((mod, pat), verbose, overall, maxidw; static, strict, dup)
    tests = updatetests!(mod, dup)
    descwidth = 0
    hasbroken = false

    id = 1
    for ts in tests
        run, id = resolve!(mod, ts, pat, verbose=verbose, id=id, strict=strict, static=static)
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
                if descx === missing
                    # we would usually have `i == 1`, but not in some rare cases;
                    # once we find an uninterpolated description, we still assume
                    # for simplicity that all remaining ones will also be uninterpolated,
                    # so we add the "repeated" annotation
                    # (it's certainly not worth it to bother being more precise about
                    # exactly which iterations are uninterpolated)
                    return dryrun_beginend(ts.desc, length(loopvalues)-i+1)
                end
                @assert descx !== missing # should be unnecessary, but there was a test below
                dryrun_beginend(descx)
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
