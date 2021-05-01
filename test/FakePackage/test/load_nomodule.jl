# no module defined, so testsets added to parentmodule
using ReTest

@testset "FakePackage: no module" begin
    @test true
end
