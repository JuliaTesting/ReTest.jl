module Included
using Hijack, Test

@testset "included testset" for _=1:1
    @test true
    push!(Hijack.RUN, 2)
    # test that `testset=true` is forwarded
    include("included_testset2.jl")
end
end
