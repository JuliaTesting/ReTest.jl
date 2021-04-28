@testset "test/sub.jl" begin
    @test true
end

module SubSub
using ReTest
__revise_mode__ = :eval

@testset "SubSub" begin
    @test true
end

end # SubSub
