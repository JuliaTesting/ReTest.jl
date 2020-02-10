module InlineTest

export @addtest, runtests, @testset, @test, @test_throws

using Test: @test, @test_throws, @testset
import Test

const INLINE_TEST = Ref{Symbol}(:__INLINE_TEST__)

__init__() = INLINE_TEST[] = gensym()

function tests(m)
    inline_test::Symbol = m ∈ (InlineTest, InlineTest.InlineTestTest) ? :__INLINE_TEST__ : INLINE_TEST[]
    if !isdefined(m, inline_test)
        @eval m $inline_test = Expr[]
    end
    getfield(m, inline_test)
end

function addtest(args::Tuple, m::Module)
    push!(tests(m), :(@testset($(args...))))
    nothing
end

macro addtest(args...)
    Expr(:call, :addtest, args, __module__)
end

function runtests(m::Module; wrap::Bool=false)
    Core.eval(m,
              if wrap
                  :(@testset $("Tests for module $m") begin
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
@addtest "test Test in sub-module" begin
    @test 1 == 1
end

end # module InlineTestTest

@addtest "self test" begin
    @assert typeof(@__MODULE__) == Module
    @test 1 != 2
    runtests(InlineTestTest)
    runtests(InlineTestTest, wrap=true)
end

end # module InlineTest
