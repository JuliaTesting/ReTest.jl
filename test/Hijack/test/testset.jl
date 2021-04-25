# testset = true
using Hijack, Test

@testset "testset" begin
    @test true
    push!(Hijack.RUN, 1)
    include("included_testset.jl")
end
