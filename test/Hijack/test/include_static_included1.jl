@testset "include_static_included1 $i" for i=1:2
    @test true
    @testset "nested include_static_included1" begin
        @test true
        push!(Hijack.RUN, 2)
        # test that `include` kwarg is forwarded
        include("include_static_included2.jl")
    end
end
