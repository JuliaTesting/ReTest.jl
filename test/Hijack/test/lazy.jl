using Test, Hijack

# called with lazy=true or lazy=:brutal
# @test_* are ignored at toplevel or in for/if/let/begin

@test false
@test_throws ArgumentError 1==1

for _=1:1
    let cond=true
        let
            if cond
                begin
                    @test false
                end
                @testset "lazy inner" begin
                    @test true
                    push!(Hijack.RUN, 1)
                end
            else
                @test false
                @testset "lazy inner 2" begin
                    @test true
                    push!(Hijack.RUN, 2)
                end
            end
        end
    end
end

@testset "lazy toplevel" begin
    @test true
    push!(Hijack.RUN, 3)
end
