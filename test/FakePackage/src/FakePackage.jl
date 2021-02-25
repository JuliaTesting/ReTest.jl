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

module Sub1

using InlineTest
using ..FakePackage: RUN

@testset "check that Sub1 is noticed by ReTest" begin
    push!(RUN, 3)
    @test true
end

end # Sub1

module Sub2
# Sub2 itself doesn't have any @testset, but has a submodule which has some

module SubSub

using InlineTest
using ...FakePackage: RUN

@testset "check that Sub2.SubSub is noticed by ReTest" begin
    push!(RUN, 4)
    @test true
end

end # SubSub
end # Sub2

end # FakePackage
