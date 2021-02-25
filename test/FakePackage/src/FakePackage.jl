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

module Submodule

using InlineTest

@testset "check that Submodule is noticed by ReTest" begin
    @test true
end

end # Submodule

end
