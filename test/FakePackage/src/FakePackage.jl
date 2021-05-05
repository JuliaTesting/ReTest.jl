module FakePackage

using InlineTest

greet() = "Hello World!"

const RUN = []

@testset "FakePackage inline begin-end" begin
    @test greet() == "Hello World!"
    @testset "inner" begin
        push!(RUN, 1)
        @test true
    end
end

@testset "FakePackage inline for" for i=1:1
    @test greet() == "Hello World!"
    push!(RUN, 2)
end

module FakeMacro ###########################################

using ReTest # check that @testset_macro is also exported by ReTest
RUN = []

macro fake_macro(x, rec::Bool=true)
    ex = quote
        @testset "macro begin x = $($x)" begin
            @test $x isa Int
            push!(FakeMacro.RUN, $x)
        end
        @testset "macro for $i" for i=1:1
            @testset "plus" begin
                @test i+$x == $x+i
            end
        end
    end
    if rec
        push!(ex.args,
              quote
              if $x == 2
                  @fake_macro 3 false
              end
              end)
    end
    ex
end

@testset_macro @fake_macro

@testset "test macro" begin
    @test true
    @fake_macro 2
end
end # FakeMacro ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

using .FakeMacro: @fake_macro

macro using_fake_macro(x)
    quote
        @testset "inner" begin
            @fake_macro $x
        end
    end
end

@testset_macro @using_fake_macro

@testset "test macro" begin
    @using_fake_macro 4
end

module Sub1 ################################################

using InlineTest
using ..FakePackage: RUN

@testset "check that Sub1 is noticed by ReTest" begin
    push!(RUN, 3)
    @test true
end

end # Sub1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

module Sub2 ################################################
# Sub2 itself doesn't have any @testset, but has a submodule which has some

module SubSub

using InlineTest
using ...FakePackage: RUN

@testset "check that Sub2.SubSub is noticed by ReTest" begin
    push!(RUN, 4)
    @test true
end

end # SubSub
end # Sub2 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

end # FakePackage
