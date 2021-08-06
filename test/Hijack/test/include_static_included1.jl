# with @eval, we test that include=:static works even for a non-toplevel
# @testset, what matters is that a testset is defined after including the file
@eval @testset "include_static_included1 $i" for i=1:2
    @test true
    @testset "nested include_static_included1" begin
        @test true
        push!(Hijack.RUN, 2)
        # test that `include` kwarg is forwarded
        include("include_static_included2.jl")
    end
end
