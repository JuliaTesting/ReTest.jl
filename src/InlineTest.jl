module InlineTest

export runtests, @testset, @test, @test_throws, Test

using Test: Test, @test, @test_throws

const INLINE_TEST = Ref{Symbol}(:__INLINE_TEST__)

__init__() = INLINE_TEST[] = gensym()

function tests(m)
    inline_test::Symbol = m ∈ (InlineTest, InlineTest.InlineTestTest) ? :__INLINE_TEST__ : INLINE_TEST[]
    if !isdefined(m, inline_test)
        @eval m $inline_test = Expr[]
    end
    getfield(m, inline_test)
end

replacetestset(x) = x

# replace unqualified `@testset` by Test.@testset
function replacetestset(x::Expr)
    x.head === :macrocall && x.args[1] === Symbol("@testset") ?
        Expr(:macrocall, Expr(:., :Test, QuoteNode(Symbol("@testset"))), map(replacetestset, x.args[2:end])...) :
        Expr(x.head, map(replacetestset, x.args)...)
end

function addtest(args::Tuple, m::Module)
    args = map(replacetestset, args)
    push!(tests(m), :(InlineTest.Test.@testset($(args...))))
    nothing
end

"""
    @testset args...

Similar to `Test.@testset args...`, but the contained tests are not run immediately,
and are instead stored for later execution, triggered by `runtests()`.
Invocations of `@testset` can be nested, but qualified invocations of
`InlineTest.@testset` can't.
Internally, `@testset` invocations are converted to `Test.@testset` at execution time.
"""
macro testset(args...)
    Expr(:call, :addtest, args, __module__)
end

"""
    runtests([m::Module]; [wrap::Bool])

Run all the tests declared in `@testset` blocks, within `m` if specified,
or within all currently loaded modules otherwise.
The `wrap` keyword specifies whether the collection of `@testset` blocks derived
from `@testset` declarations should be grouped within a top-level `@testset`.
The default is `wrap=false` when `m` is specified, `true` otherwise.

Note: this function executes each (top-level) `@testset` block using `eval` *within* the module
in which it was written (e.g. `m`, when specified).
"""
function runtests(m::Module; wrap::Bool=false)
    Core.eval(m,
              if wrap
                  :(InlineTest.Test.@testset $("Tests for module $m") begin
                    $(tests(m)...)
                    end)
              else
                  Expr(:block, tests(m)...)
              end)
    nothing
end

function runtests(; wrap::Bool=true)
    foreach(values(Base.loaded_modules)) do m
        if isdefined(m, INLINE_TEST[]) # will automatically skip InlineTest and InlineTest.InlineTestTest
            ts = runtests(m, wrap=wrap)
        end
    end
end


module InlineTestTest

using ..InlineTest
@testset "test Test in sub-module" begin
    @test 1 == 1
end

end # module InlineTestTest

@testset "self test" begin
    @assert typeof(@__MODULE__) == Module
    @test 1 != 2
    runtests(InlineTestTest)
    runtests(InlineTestTest, wrap=true)
end

end # module InlineTest
