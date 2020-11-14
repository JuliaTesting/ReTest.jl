module ReTest

export runtests, @testset, InlineTest

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

using InlineTest: InlineTest, get_tests, TESTED_MODULES, INLINE_TEST

include("testset.jl")

using .Testset: Testset, @testsetr


mutable struct TestsetExpr
    desc::Union{String,Expr}
    loops::Union{Expr,Nothing}
    parent::Union{TestsetExpr,Nothing}
    children::Vector{TestsetExpr}
    strings::Vector{String}
    loopvalues::Any
    run::Bool
    body::Expr

    TestsetExpr(desc, loops, parent) = new(desc, loops, parent, TestsetExpr[], String[])
end

isfor(ts::TestsetExpr) = ts.loops !== nothing
isfinal(ts::TestsetExpr) = isempty(ts.children)

# replace unqualified `@testset` by TestsetExpr
function replace_ts(x::Expr, parent)
    if x.head === :macrocall && x.args[1] === Symbol("@testset")
        @assert x.args[2] isa LineNumberNode
        ts = parse_ts(Tuple(x.args[3:end]), parent)
        parent !== nothing && push!(parent.children, ts)
        ts
    else
        body = map(z -> replace_ts(z, parent), x.args)
        Expr(x.head, body...)
    end
end

replace_ts(x, _) = x

# create a TestsetExpr from @testset's args
function parse_ts(args::Tuple, parent=nothing)
    length(args) == 2 || error("unsupported @testset")

    desc = args[1]
    desc isa String || Meta.isexpr(desc, :string) || error("unsupported @testset")

    body = args[2]
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

    ts = TestsetExpr(desc, loops, parent)
    ts.body = replace_ts(tsbody, ts)
    ts
end

"""
    @testset args...

Similar to `Test.@testset args...`, but the contained tests are not run immediately,
and are instead stored for later execution, triggered by `runtests()`.
Invocations of `@testset` can be nested, but qualified invocations of
`ReTest.@testset` can't.
Internally, `@testset` invocations are converted to `Test.@testset` at execution time.
"""
macro testset(args...)
    # this must take effect at compile/run time rather than parse time, e.g.
    # if the @testset if in a `if false` branch
    # TODO: test that
    quote
        ts = parse_ts($args)
        push!(get_tests($__module__), ts)
        nothing
    end
end

function resolve!(mod::Module, ts::TestsetExpr, rx::Regex, force::Bool=false)
    strings = empty!(ts.strings)
    desc = ts.desc
    ts.run = force || isempty(rx.pattern)
    ts.loopvalues = nothing # unnecessary ?

    parentstrs = ts.parent === nothing ? [""] : ts.parent.strings

    if desc isa String
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
        end
        for x in xs # empty loop if eval above threw
            ts.run && break
            Core.eval(mod, Expr(:(=), loops.args[1], x))
            descx = Core.eval(mod, desc)::String
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
        run |= resolve!(mod, tsc, rx, ts.run)
    end
    ts.run = run
end

# convert a TestsetExpr into an actually runnable testset
function make_ts(ts::TestsetExpr, rx::Regex)
    ts.run || return nothing

    if isfinal(ts)
        body = ts.body
    else
        body = make_ts(ts.body, rx)
    end
    if ts.loops === nothing
        quote
            let $(Testset.REGEX[]) = $rx,
                $(Testset.FINAL[]) = $(isfinal(ts))

                InlineTest.@testsetr $(ts.desc) begin
                    $(body)
                end
            end
        end
    else
        loopvals = something(ts.loopvalues, ts.loops.args[2])
        quote
            let $(Testset.REGEX[]) = $rx,
                $(Testset.FINAL[]) = $(isfinal(ts))

                InlineTest.@testsetr $(ts.desc) for $(ts.loops.args[1]) in $loopvals
                    $(body)
                end
            end
        end
    end
end

make_ts(x, rx) = x
make_ts(ex::Expr, rx) = Expr(ex.head, map(x -> make_ts(x, rx), ex.args)...)

"""
    runtests([m::Module], pattern = r""; [wrap::Bool])

Run all the tests declared in `@testset` blocks, within `m` if specified,
or within all currently loaded modules otherwise.
The `wrap` keyword specifies whether the collection of `@testset` expressions
should be grouped according to the parent modules within a top-level `@testset`.
The default is `wrap=false` when `m` is specified, `true` otherwise.

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

If the passed `pattern` is a string, then it is wrapped in a `Regex` and must
match literally the subjects.
This means for example that `"a|b"` will match a subject like `"a|b"` but not like `"a"`
(only in Julia versions >= 1.3; in older versions, the regex is simply created as
`Regex(pattern)`).

Note: this function executes each (top-level) `@testset` block using `eval` *within* the
module in which it was written (e.g. `m`, when specified).
"""
function runtests(mod::Module, pattern::Union{AbstractString,Regex} = r""; wrap::Bool=false)
    regex = pattern isa Regex ? pattern :
        if VERSION >= v"1.3"
            r"" * pattern
        else
            Regex(pattern)
        end

    testsets = []

    tests = get_tests(mod)

    for idx in eachindex(tests)
        ts = tests[idx]
        if !(ts isa TestsetExpr)
            ts = tests[idx] = parse_ts(ts)
        end
        run = resolve!(mod, ts, regex)
        run || continue
        mts = make_ts(ts, regex)
        if wrap
            push!(testsets, mts)
        else
            Core.eval(mod, mts)
        end
    end
    if wrap
        Core.eval(mod,
                  quote
                      ReTest.Test.@testset $("Tests for module $mod") begin
                          $(testsets...)
                      end
                  end)
    end
    nothing
end

function runtests(pattern::Union{AbstractString,Regex} = r""; wrap::Bool=true)
    # TESTED_MODULES is not up-to-date w.r.t. package modules which have
    # precompilation, so we have to also look in Base.loaded_modules
    # TODO: look recursively in "loaded modules" which use ReTest for sub-modules
    for m in unique(Iterators.flatten((values(Base.loaded_modules), TESTED_MODULES)))
        if isdefined(m, INLINE_TEST[])
            # will automatically skip ReTest and ReTest.ReTestTest
            runtests(m, pattern, wrap=wrap)
        end
    end
end

function partialize(r::Regex)
    if r.match_options & (Base.PCRE.PARTIAL_HARD | Base.PCRE.PARTIAL_SOFT) == 0
        Regex(r.pattern, r.compile_options, r.match_options | Base.PCRE.PARTIAL_SOFT)
    else
        r
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
    runtests(ReTestTest)
    runtests(ReTestTest, wrap=true)
end

end # module ReTest
