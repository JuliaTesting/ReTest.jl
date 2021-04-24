module Simple
using Hijack, Test

@testset "simple" begin
    @test true
    push!(Hijack.RUN, 4)
end

end
