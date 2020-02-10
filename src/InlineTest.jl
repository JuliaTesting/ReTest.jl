module InlineTest

export @addtest, runtests, @testset, @test, @test_throws

using Test: @test, @test_throws, @testset
import Test

function tests(m)
    if !isdefined(m, :__INLINE_TESTS__)
        @eval m __INLINE_TESTS__ = Dict{AbstractString, Expr}()
    end
    m.__INLINE_TESTS__
end

function addtest(args::Tuple, m::Module)
    n = findfirst(a -> a isa String, args)
    desc = n == nothing ? nothing : args[n]
    desc != nothing && haskey(tests(m), desc) && @warn("Test $desc already defined for module $(string(m)), overwriting")
    if desc == nothing
        i = 1
        while haskey(tests(m), "anonymous test $i")
            i += 1
        end
        desc = "anonymous test $i"
    end
    tests(m)[desc] = :(@testset($(args...)))
    nothing
end

macro addtest(args...)
    Expr(:call, :addtest, args, __module__)
end

function runtests(m::Module)
    tss = collect(values(tests(m)))
    tsm = :(@testset $("Tests for module $m") begin
               $(tss...)
           end)
    Core.eval(m, tsm)
    nothing
end

function runtests()
    foreach(values(Base.loaded_modules)) do m
        if isdefined(m, :__INLINE_TESTS__) && m != InlineTest && m != InlineTest.InlineTestTest
            ts = runtests(m)
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
end

end # module InlineTest
