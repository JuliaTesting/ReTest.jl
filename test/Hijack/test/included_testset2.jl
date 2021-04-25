@testset "included testset 2" begin
    @test true
    push!(Hijack.RUN, 3)
end
