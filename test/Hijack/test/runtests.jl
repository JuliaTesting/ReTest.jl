using Test, Hijack

@test true

@testset "runtests" begin
    @test true
end

# `include` within nested control-flow constructs
begin
    let cond=true
        let
            if cond
                for _=1:1
                    include("included.jl")
                end
            end
            if false
                include("not-a-file.jl")
            else
                @test true
            end
        end
    end
end
