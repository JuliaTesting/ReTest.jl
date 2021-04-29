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
                for i=1:1
                    while i==1
                        try
                            for my_test in ["included.jl"]
                                let my_test2 = my_test
                                    include(my_test2)
                                end
                            end
                            error(":)")
                        catch
                            include("simple.jl")
                        finally
                            include("simple2.jl")
                        end
                        i+=1
                    end
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
