@testset "include_static_included2" begin
    @test true
    push!(Hijack.RUN, 3)
end
