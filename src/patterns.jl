const PatternX = Union{Pattern, Regex, Integer}

## Patterns

### And

struct And <: Pattern
    xs::Vector{PatternX}
end

And() = And(PatternX[])
and(xs...) = And(PatternX[make_pattern(x) for x in xs])

==(a::And, b::And) = a.xs == b.xs


### Or

struct Or <: Pattern
    xs::AbstractArray{<:PatternX}
end

Or() = Or(PatternX[])
or(xs...) = Or(PatternX[make_pattern(x) for x in xs])

==(a::Or, b::Or) = a.xs == b.xs


### Not

struct Not <: Pattern
    x::PatternX
end

==(a::Not, b::Not) = a.x == b.x


### Reachable

struct Reachable <: Pattern
    x::PatternX
end

==(a::Reachable, b::Reachable) = a.x == b.x


### depth

struct Depth <: Pattern
    d::Int
end


### Interpolated

struct Interpolated <: Pattern end


### pass/fail

struct Pass <: Pattern end
const pass = Pass()

struct Fail <: Pattern end
const fail = Fail()


## alwaysmatches

alwaysmatches(pat::And, d) = all(p -> alwaysmatches(p, d), pat.xs)

alwaysmatches(pat::Or, d) =
    if pat.xs isa AbstractArray{<:Integer}
        false # special case for huge unit ranges; locally, this optimization seems
              # unnecessary, i.e. alwaysmatches(Or(1:10...0)) is constant time anyway,
              # but on CI, the any(...) below takes tooooo long
    else
        any(p -> alwaysmatches(p, d), pat.xs)
    end

alwaysmatches(::Not, _) = false
alwaysmatches(::Interpolated, _) = false
alwaysmatches(rx::Regex, _) = isempty(rx.pattern)
alwaysmatches(id::Integer, _) = false
alwaysmatches(pat::Reachable, d) = alwaysmatches(pat.x, d)
alwaysmatches(dep::Depth, d) = dep.d == d
alwaysmatches(::Pass, _) = false
alwaysmatches(::Fail, _) = false


## matches

matches(pat::And, x, ts) = all(p -> matches(p, x, ts), pat.xs)

matches(pat::Or, x, ts) =
    if pat.xs isa AbstractUnitRange{<:Integer} && minimum(pat.xs) >= 0
        ts.id ∈ pat.xs # this is optimised, i.e. it's not O(n)
    else
        any(p -> matches(p, x, ts), pat.xs)
    end

matches(pat::Not, x, ts) = !matches(pat.x, x, ts)
matches(::Interpolated, x::Union{Missing,AbstractString}, ts) = x !== missing
matches(rx::Regex, x, _) = occursin(rx, x)
matches(rx::Regex, ::Missing, ts) = alwaysmatches(rx, tsdepth(ts)) | missing
matches(pat::Integer, _, ts) =
    @inbounds pat >= 0 ?
        pat == ts.id :
        pat != -ts.id

function matches(pat::Reachable, desc, ts)
    if desc !== missing
        desc = SubString(desc)
    end
    m = false
    while true
        m |= matches(pat.x, desc, ts)
        m === true && return true
        ts.parent === nothing && break
        ts = ts.parent
        if desc !== missing
            desc = SubString(desc, 1, findlast('/', desc)-1)
        end
    end
    m
end

matches(d::Depth, _, ts) = d.d == tsdepth(ts)

matches(::Pass, subj::AbstractString, ts) = get(ts.pastresults, subj, false)
matches(::Fail, subj::AbstractString, ts) = !get(ts.pastresults, subj, true)
# TODO: test method below
matches(::Union{Pass,Fail}, ::Missing, ts) =
    isempty(ts.pastresults) ? false : missing


## make_pattern

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


## hasinteger

hasinteger(::Regex) = false
hasinteger(::Integer) = true
hasinteger(pat::Union{And,Or}) = any(hasinteger, pat.xs)
hasinteger(pat::Not) = hasinteger(pat.x)
hasinteger(::Interpolated) = false
hasinteger(pat::Reachable) = hasinteger(pat.x)
hasinteger(::Depth) = false
hasinteger(::Pass) = false
hasinteger(::Fail) = false


## exported pattern functions & singletons


"""
    not(pattern)

Create an object suitable for filtering testsets (in the [`retest`](@ref) function),
which "negates" the meaning of `pattern`: a testset matches `not(pattern)`
if and only if it doesn't match `pattern`.

For example `not("a")` matches any testset whose subject doesn't contain `"a"`,
and `not(1:3)` matches all the testsets but the first three of a module.

If `pattern` is an integer or a `ReTest` object (i.e. not a `AbstractString`,
`Regex`, `Tuple` or `AbstractArray`), `not(pattern)` can be expressed as `-pattern`.

`String` patterns can also be negated by prepending `'-'`, see [`retest`](@ref)
for details.
"""
not(x) = Not(make_pattern(x))

Base.:-(x::Pattern) = not(x)


"""
    interpolated

Singleton pattern which matches any testset whose subject can be interpolated
"statically", i.e. at filtering time before testset are actually run.
Non-inferrable subjects include those constructed from descriptions
containing interpolated values which can't be known until run time.
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
For example, one can see the list of non-interpolated subjects as follows with
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
subjects can't be interpolated, but still include those which are
relevant. For example, assume we want to run testsets `1` and `2`,
while excluding other testsets with uninterpolated subjects:
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
its subject is unknown at this point.

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
  outer    |      2
    inner  |      1
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


"""
    reachable(pattern)

Create a filtering pattern which matches any testset matching `pattern`
or whose parent testset, if any, matches `reachable(pattern)`.
In other words, if a testset matches `pattern`, all its recursive
nested testsets will also match.

When `pattern::String`, `reachable(pattern)` has the same effect as
`pattern`, because the subject of a testset is contained in the
subjects of all its nested testsets. So `reachable` is typically
useful when `pattern` is an integer.

# Examples
```julia
julia> module T
       using ReTest
       @testset "a" verbose=true begin
           @test true
           @testset "b" begin
               @test true
           end
       end
       @testset "c" begin
           @test true
       end
       end;

julia> retest(T, reachable(1), dry=true)
1| a
2|   b

julia> retest(T, not(reachable(1)), dry=true)
3| c
```

Note that the algorithm for `reachable` is currently not optimized, i.e.
it will match `pattern` against all parents of a testset until success,
even when this match was already performed earlier (i.e. the result
of matching against `pattern` is not cached).

Also, in the current implementation, the subject of a parent testset is
inferred from the subject of a testset, by chopping off the last component,
determined by the last occurrence of `'/'`. This has two consequences. It will
produce incorrect results if the description of a testset contains `'/'`, and
also, with [`interpolated`](@ref) when the subject is "unknown" due to
un-interpolated descriptions. Consider the following example:

```julia
julia> module Fail
       using ReTest
       @testset "a" begin
           x = 1
           @testset "b\$x" begin
               @testset "c" begin end
           end
       end
       end;

julia> retest(Fail, reachable(1), verbose=9, dry=true)
1| a
2|   "b\$(x)"
3|     c

julia> retest(Fail, reachable(interpolated), verbose=9, dry=true)
1| a
```

Here, both testsets with id `2` and `3` have an unknown subject (at
filtering time), which prevents the algorithm to detect that one of their
parents (testset `1`) actually has an "interpolated" description.

On the other hand, even with these unknown subjects, something like
`reachable("a")` would work as expected:

```julia
julia> retest(Fail, reachable("a"), verbose=9, dry=true)
1| a
2|   "b\$(x)"
3|     c

julia> retest(Fail, reachable("a"), verbose=9, dry=true, static=true)
1| a
```

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
function reachable end

if VERSION >= v"1.3"
    reachable(x) = Reachable(make_pattern(x))
end

"""
    depth(d::Integer)

Create a pattern which matches testsets at "depth" `d`.
Toplevel testsets have depth `1`, their direct children
(nested testsets) depth `2`, and so on.

# Examples

```julia
julia> module Depth
       using ReTest
       @testset "1" begin
           @testset "2" begin
               @testset "3" begin end
           end
           @testset "4" begin end
       end
       end;

julia> Depth.runtests(dry=true, verbose=3, depth(2))
1| 1
2|   2
4|   4

julia> Depth.runtests(dry=true, verbose=3, depth(3))
1| 1
2|   2
3|     3

julia> Depth.runtests(dry=true, verbose=3, reachable(depth(2)))
1| 1
2|   2
3|     3
4|   4

julia> Depth.runtests(dry=true, verbose=3, depth.(2:3))
1| 1
2|   2
3|     3
4|   4
```
"""
depth(x::Integer) = Depth(Int(x))

"""
    pass
    fail

Filtering patterns which match any testset which already ran,
succesfully for `pass` or with at least one error for `fail`.
The pattern `[pass, fail]` therefore matches any testset
which already ran.
"""
pass, fail
