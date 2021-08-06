module InlineTest

@static if isdefined(Base, :Experimental) &&
             isdefined(Base.Experimental, Symbol("@optlevel"))
    Base.Experimental.@optlevel 0
end

export @testset, @testset_macro

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


# it seems difficult to use gensym here, as other gensym calls (from other
# modules) can lead to collisions or mismatches, whether we statically
# initialize INLINE_TEST or at __init__ time; so let's use a long enough
# static random hexadecimal string
const INLINE_TEST = Symbol("##InlineTest-01b48f5c342f65df7fcd07f28f0d2cacbb09f0a0")

const TESTED_MODULES = Union{Module,Nothing}[]
const TESTSET_MACROS = Symbol[]

get_tests(m::Module) = getfield(m, INLINE_TEST).tests

function register(m::Module, macros::Vector{Symbol})
    push!(TESTED_MODULES, m)
    foreach(register_macro, macros)
end

register_macro(mac::Symbol) = mac ∉ TESTSET_MACROS && push!(TESTSET_MACROS, mac)
# uniquify just in case, to keep TESTSET_MACROS as short as possible


function retest end

"""
    @testset args...

Similar to `Test.@testset args...`, but the contained tests are not run
immediately, and are instead stored for later execution, triggered by
[`retest()`](@ref) or `runtests()`.

Besides the `@testset` body (last argument) and a description string,
arguments of `@testset` can be:
* the `verbose` option, with a *literal* boolean value (e.g.
  `verbose=true`)
* a literal symbol, serving as a label which can be used for
  testset filtering (see [`retest`](@ref)'s docstring for details).
  All nested testsets inherit such labels.

Invocations of `@testset` can be nested, but qualified invocations of
`ReTest.@testset` can't.

A `@testset` can contain a nested `Test.@testset`, or call a function
which defines a `Test.@testset`: in this case, the `Test.@testset`
will be run whenever the parent testset is run, but `retest` won't
know about it: it won't be taken into account
during the filtering phase, and won't be printed in dry mode.

Internally, `@testset` expressions are converted to an equivalent of
`Test.@testset` at execution time.
"""
macro testset(args...)
    let mod = __module__
        while !isdefined(mod, :runtests)
            @eval mod begin
                """
                    $($mod).runtests(pattern...; kwargs...)

                Equivalent to `ReTest.retest($($mod), pattern...; kwargs...)`.
                This function is defined automatically in any module containing
                a `@testset`, possibly nested within submodules.
                """
                function runtests(specs...; kwargs...)
                    $retest($mod, specs...; kwargs...)
                end
            end
            mod = parentmodule(mod)
        end
    end
    get_inline_mod!(__module__)
    # this must take effect at compile/run time rather than parse time, e.g.
    # if the @testset if in a `if false` branch
    # TODO: test that
    quote
        push!(get_tests($__module__).news, (ts=$args, source=$(QuoteNode(__source__))))
        nothing
    end
end

function get_inline_mod!(mod)::Module
    if isdefined(mod, INLINE_TEST)
        getfield(mod, INLINE_TEST)
    else
        # Credit to Takafumi Arakaki for the idea of creating a submodule
        # within modules using InlineTest in order to be able to define
        # an `__init__` function which does the registration
        # (as we can't directly create/modify `__init__` for the given module).
        @eval mod module $INLINE_TEST
            const tests = (tests=[], news=[], map=Dict{Union{String,Expr},Int}())
            const TESTSET_MACROS = Symbol[]
            __init__() = $register($mod, TESTSET_MACROS)
        end
    end
end

"""
    @testset_macro @mac

Declare `@mac` as a macro which must be expanded statically by `retest` so that
contained `@testset`s can be discovered.

Consider this pattern with `Test` which factors out testsets in a function:

```julia
using Test

function test_iseven(x)
    @testset "iseven \$x" begin
        @test iseven(x)
    end
end

@testset "test \$x" for x=2:2:4
    test_iseven(x)
end
```

This doesn't translate directly with `ReTest`, as the call to `test_iseven` will be
performed at run-time, and will end up declaring a new `@testset "iseven \$x"` at
toplevel (this is a problem similar to having `include` inside testsets).
So on the first run of `retest()`, no `@test` is run, and on the second one,
it fails because `x` is not defined at global scope.

The alternative is to turn `test_iseven` into a macro and declare it with `@testset_macro`:

```julia
using ReTest

macro test_iseven(x)
    quote
        @testset "iseven \$(\$x)" begin
            @test iseven(\$x)
        end
    end
end

@testset_macro @test_iseven

@testset "test \$x" for x=2:2:4
    @test_iseven(x)
end
```
Then, running `retest("iseven", verbose=2)` gives:
```
                    Pass
test 2          |      1
  iseven 2      |      1
test 4          |      1
  iseven 4      |      1
Main            |      2
```
"""
macro testset_macro(mac)
    Meta.isexpr(mac, :macrocall, 2) ||
        error("usage: @testset_macro @macro_to_be_registered")
    inline_mod = get_inline_mod!(__module__)
    mac = QuoteNode(mac.args[1]::Symbol)
    quote
        push!($(inline_mod.TESTSET_MACROS), $mac)
        # need also to register right away, e.g. in case __init__ was already run
        InlineTest.register_macro($mac)
        nothing
    end
end

end # InlineTest
