@test true

@testset "sub" begin
    @test true
    push!(Hijack.RUN, 1)
end
