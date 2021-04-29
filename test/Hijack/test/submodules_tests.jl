# this test file is called directly by hijack, could be a runtests.jl file
# we test that included submodules have Revise working

using Test
include("submodule.jl")

RUN = []
@testset "submodules_tests" begin
    push!(RUN, 1)
end
