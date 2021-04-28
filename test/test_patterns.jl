module TestPatterns
using Test

using ReTest: and, or, not, interpolated

@testset "patterns: ==" begin
    basics = [and(), or(), not(0), interpolated, 0, r""]
    for a = basics, b = basics
        if a === b
            @test a == b
        else
            @test a != b
            for f in (and, or)
                @test f(a) == f(a)
                @test f(a) != f(b)
                @test f(a, b) == f(a, b)
                @test f(a, b) != f(b, a)
                @test f(and(a, b), or(b, a)) == f(and(a, b), or(b, a))
                @test f(and(a, b), or(a, b)) != f(and(a, b), or(b, a))
                @test f(not(a), and(a, b)) == f(not(a), and(a, b))
                @test f(and(a, b), not(a)) != f(not(a), and(a, b))
                @test f(not(a), r"a", and(a, b)) == f(not(a), r"a", and(a, b))
                @test f(not(a), r"a", and(a, b)) != f(not(a), r"b", and(a, b))
            end
            @test not(a) == not(a)
            @test not(a) != not(b)
            @test not(not(a)) == not(not(a))
        end
    end
end

end # TestPatterns
