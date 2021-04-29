# this is not testing hijack(), but load(revise=true), but is in this
# Hijack package because it's there that Revise gets loaded
module HijackTests # ReTest.load will search for this name
include("../../setup.jl")

using ReTest, .Trace

@testset "Hijack: load(revise=true)" begin
    @test true
    trace(1)
end
end # HijackTests

nothing # including this file must not return the module
