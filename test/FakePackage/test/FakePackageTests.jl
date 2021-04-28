module FakePackageTests
# this is used to test `load` keyword of retest, but also to manually check that Revise
# works as expected: e.g. do `using Revise, FakePackageTests` and `retest(FakePackageTests)`
# multiple times, while changing the testcode in this file or the included one "sub.jl"

using ReTest
__revise_mode__ = :eval

@testset "toplevel" begin
    @test true
end

module Sub
using ReTest
__revise_mode__ = :eval

@testset "Sub" begin
    @test true
end

include("./"*"sub.jl")

end # Sub
end # FakePackageTests
