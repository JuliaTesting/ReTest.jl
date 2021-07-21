module TestPatterns
using Test

using ReTest: and, or, not, interpolated, reachable, depth, pass, fail, iter
import ReTest

struct MockTestset
    id
    marks
    parent
    iter

    MockTestset() = new(rand(1:typemax(Int)), ReTest.Marks(), nothing, 1)
end

ReTest.tsdepth(::MockTestset) = 1

const basic_patterns = [and(), or(), not(0), interpolated, 0, r"", :label,
                        depth(2), pass, fail, iter(1)]
VERSION >= v"1.3" && push!(basic_patterns, reachable(1))

@testset "patterns: ==" begin
    for a = basic_patterns, b = basic_patterns
        if a === b
            @test a == b
            if !(a isa Regex || a isa Integer || a isa Symbol)
                @test -a == not(a)
            end
        else
            @test a != b
            for f in (and, or)
                @test f(a) == f(a)
                @test f(a) == f(deepcopy(a))
                @test f(a) != f(b)
                @test f(a, b) == f(a, b)
                @test f(a, b) == f(deepcopy(a), deepcopy(b))
                @test f(a, b) != f(b, a)
                @test f(and(a, b), or(b, a)) == f(and(a, b), or(b, a))
                @test f(and(a, b), or(a, b)) != f(and(a, b), or(b, a))
                @test f(not(a), and(a, b)) == f(not(a), and(a, b))
                @test f(and(a, b), not(a)) != f(not(a), and(a, b))
                @test f(not(a), r"a", and(a, b)) == f(not(a), r"a", and(a, b))
                @test f(not(a), r"a", and(a, b)) != f(not(a), r"b", and(a, b))
            end
            @test not(a) == not(a)
            @test not(a) == not(deepcopy(a))
            @test not(a) != not(b)
            @test not(not(a)) == not(deepcopy(not(a)))
            if VERSION >= v"1.3"
                @test reachable(a) == reachable(a)
                @test reachable(a) == reachable(deepcopy(a))
                @test reachable(a) != reachable(b)
                @test reachable(reachable(a)) == reachable(deepcopy(reachable(a)))
            end
        end
    end
end

@testset "patterns: interface" begin
    for a in basic_patterns
        @test ReTest.matches(a, "a", MockTestset()) isa Union{Missing, Bool}
        @test ReTest.matches(a, missing, MockTestset()) isa Union{Missing, Bool}
        @test ReTest.alwaysmatches(a, 1) isa Bool
        @test ReTest.has(a, Integer) isa Bool
        @test ReTest.has(a, ReTest.Iter) isa Bool
    end
end

@testset "patterns: not" begin
    pats = [or(1, 3), and(1, r"a"), not(1), interpolated, depth(3)]
    VERSION >= v"1.3" && push!(pats, reachable("c"))
    for p ∈ pats
        @test -p == not(p)
    end
end

end # TestPatterns
