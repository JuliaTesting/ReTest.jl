# this is not testing hijack(), but load(revise=true), but is in this
# Hijack package because it's there that Revise gets loaded

load_revise_function() = 1

module HijackTests # ReTest.load will search for this name
include("../../setup.jl")

load_revise_function() = 1

using ReTest, .Trace

@testset "Hijack: load(revise=true)" begin
    @test true
    trace(1)
end
end # HijackTests

nothing # including this file must not return the module
