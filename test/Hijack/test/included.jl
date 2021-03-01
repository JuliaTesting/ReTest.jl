makepath(p) = joinpath("subdir", p)

@testset "included" begin
    @test true
end

# test that ReTest can handle non-literal arguments in include
SUB="sub"
include(makepath("$SUB.jl"))
