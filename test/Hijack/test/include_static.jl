# include = :static
using Hijack, Test

custom_include_function(f) = include(f)

@testset "include_static" begin
    @test true
    push!(Hijack.RUN, 1)
    custom_include_function("include_static_included1.jl")
end
