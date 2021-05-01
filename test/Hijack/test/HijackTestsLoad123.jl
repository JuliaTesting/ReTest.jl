# this doesn't test `hijack` stuff, just `load` stuff. We put it here
# because we test Revise stuff
module HijackTestsLoad1
f() = 1

# HijackTestsLoad1 is returned by load() even if only a submodule uses ReTest
module Sub
using ReTest
@testset "HijackTestsLoad1" begin end
end
end

module HijackTestsLoad2
f() = 1
using ReTest
@testset "HijackTestsLoad2" begin end
end

# not using ReTest
module HijackTestsLoad3
f() = 1
end
