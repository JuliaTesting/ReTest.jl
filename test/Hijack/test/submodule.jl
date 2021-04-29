module SubModule
using Test

RUN = []
@testset "A" begin
    push!(RUN, 1)
end

if VERSION >= v"1.6"
    include("subsubmodule.jl")
end
end # SubModule
