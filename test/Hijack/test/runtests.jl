using Test, Hijack

@test true

@testset "runtests" begin
    @test true
end

include("included.jl")
