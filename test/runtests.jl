using ReTest

module M
using ReTest

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
check("/b", ["b1", "b2"])
check("b1", ["b1"])
check("c1", []) # "c1" is *not* partially matched against "/c/"
check("c/1", []) # "c" is partially matched, but nothing fully matches afterwards
check("c/d1", [])
check("c/d", ["c", "d"])
check("/c", ["c", "d", "e1", "e2"])
check(".*d", ["c", "d"])
check(".*(e1|e2)", ["c", "e1", "e2"])
check("f1", ["f1", "g", "h1", "h2"])
check("f",  ["f1", "g", "h1", "h2"])
check(".*g", ["f1", "g"])
check(".*h", ["f1", "h1", "h2"])
check(".*h1", ["f1", "h1"])
check(".*h\$", [])

runtests(M, wrap=true) # TODO: more precise tests
runtests(M, wrap=false)

module N
using ReTest

RUN = []

# testing non-final non-toplevel testsets
@testset "i" begin
    push!(RUN, "i")
    @test true

    @testset "j" begin
        push!(RUN, "j")
        @test true

        @testset "k" begin
            push!(RUN, "k")
            @test true
        end
    end

    @testset "l$i" for i=1:1
        push!(RUN, "l$i")
        @test true

        @testset "m" begin
            push!(RUN, "m")
            @test true
        end
    end
end

function check(rx, list)
    empty!(RUN)
    runtests(N, Regex(rx))
    @test sort(RUN) == sort(list)
end
end

import .N
N.check(".*j1", [])
N.check(".*j/1", [])
N.check("^/i/j0", [])
N.check("^/i/l10", [])

module P
using ReTest

RUN = []

# testing non-final non-toplevel testsets
@testset "a" begin
    push!(RUN, "a")
    @test true

    @testset "b" begin
        push!(RUN, "b")
        @test true
    end

    @testset "b|c" begin
        push!(RUN, "b|c")
        @test true
    end
end

function check(rx, list)
    empty!(RUN)
    runtests(P, rx)
    @test sort(RUN) == sort(list)
end
end

import .P # test ReTest's wrapping of non-regex patterns
P.check("b", ["a", "b", "b|c"]) # an implicit prefix r".*" is added

if VERSION >= v"1.3"
    P.check("b|c", ["a", "b|c"]) # "b" is not matched
end

RUN = []
@testset "toplevel" begin
    # this tests that the testset is run exactly once
    # Main is special here, as it's both in Base.loaded_modules
    # and it gets registered automatically in ReTest.TESTED_MODULES
    push!(RUN, "toplevel")
    @test true
end

runtests()
@test RUN == ["toplevel"]

runtests(r"^/f1") # just test that a regex can be passed

module Loops
using ReTest

RUN = []

@testset "parent" begin
    a = 1
    b = 2

    @testset "local$i" for i in (a, b)
        push!(RUN, i)
        @test true
        @testset "sub" begin
            push!(RUN, 0)
            @test true

            @testset "final" begin
                @test true
                push!(RUN, -1)
            end
        end
    end
end
end

@test_logs (:warn, r"could not evaluate testset-for iterator.*") runtests(Loops, r"asd")
@test Loops.RUN == [1, 0, 2, 0]
empty!(Loops.RUN)
@test_logs (:warn, r"could not evaluate testset-for iterator.*") runtests(Loops)
@test Loops.RUN == [1, 0, -1, 2, 0, -1]
