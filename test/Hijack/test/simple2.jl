@testset "simple2" begin
    @test true
    push!(Hijack.RUN, 5)
end
