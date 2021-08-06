# include = :static
using Hijack, Test

@testset "include_static" begin
    @test true
    push!(Hijack.RUN, 1)
    include("include_static_included1.jl")
end
