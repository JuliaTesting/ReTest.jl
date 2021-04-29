module SubModule
using Test

RUN = []
@testset "A" begin
    push!(RUN, 1)
end

end # SubModule
