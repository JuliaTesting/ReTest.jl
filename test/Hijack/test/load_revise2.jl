# this is not testing hijack(), but load(revise=true), but is in this
# Hijack package because it's there that Revise gets loaded
module HijackTestsFail # ReTest.load will search for HijackTests, so this fails
using ReTest

@testset "Hijack: load(revise=true)" begin
    @test true
end
end # HijackTests

nothing # including this file must not return the module
