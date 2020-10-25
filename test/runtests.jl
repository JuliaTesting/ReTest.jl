using InlineTest

module M
using InlineTest

RUN = []

@testset "a" begin
    push!(RUN, "a")
    @test true
end

@testset "b$i" for i=1:2
    push!(RUN, "b$i")
    @test true
end

@testset "c" begin
    push!(RUN, "c")
    @test true

    @testset "d" begin
        push!(RUN, "d")
        @test true
    end

    @testset "e$i" for i=1:2
        push!(RUN, "e$i")
        @test true
    end
end

@testset "f$i" for i=1:1
    push!(RUN, "f$i")
    @test true

    @testset "g" begin
        push!(RUN, "g")
        @test true
    end

    @testset "h$i" for i=1:2
        push!(RUN, "h$i")
        @test true
    end
end

function check(rx, list)
    empty!(RUN)
    runtests(M, Regex(rx))
    @test sort(RUN) == sort(list)
end
end

# we don't put these checks in a @testset, as this would modify filtering logic
# (e.g. with `@testset "filtering" ...`, testset subjects would start with "/filtering/...")
using .M: check
check("a", ["a"])
check("a1", []) # testset is "final", so we do a full match
check("b", ["b1", "b2"])
check("b1", ["b1"])
check("c1", ["c"]) # "c" is partially matched, but nothing fully matches afterwards
check("c/d1", ["c"])
check("c/d", ["c", "d"])
check(".*d", ["c", "d", "f1"])
check(".*(e1|e2)", ["c", "e1", "e2", "f1"])
check("f1", ["f1", "g", "h1", "h2"])
check("f",  ["f1", "g", "h1", "h2"])
check(".*g", ["c", "f1", "g"])
check(".*h", ["c", "f1", "h1", "h2"])
check(".*h1", ["c", "f1", "h1"])
check(".*h\$", ["c", "f1"])

runtests(M, wrap=true) # TODO: more precise tests
runtests(M, wrap=false)
