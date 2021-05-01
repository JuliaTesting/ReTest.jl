# this is testing retest(load=true), so not hijack stuff, but we already tested
# load=true for FakePackage, here we want to test another setting with two
# included modules

module HijackLoadTrueTests
using ReTest
@testset "HijackLoadTrueTests" begin end
end

module HijackLoadTrueTests2
using ReTest
@testset "HijackLoadTrueTests2" begin end
end
