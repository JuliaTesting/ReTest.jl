using Pkg
Pkg.activate("ReTest")
Pkg.develop(PackageSpec(path="../InlineTest"))

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

@testset verbose=true "c" begin
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

@testset "f$i" verbose=true for i=1:1
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

innertestsets = ["d", "e1", "e2", "g", "h1", "h2"]

function check(rx, list)
    empty!(RUN)
    retest(M, Regex(rx))
    @test RUN == list
    mktemp() do path, io
        redirect_stdout(io) do
            retest(M, Regex(rx), dry=true)
        end
        seekstart(io)
        expected = map(list) do t
            if t in innertestsets
                "  " * t
            else
                t
            end
        end
        expected = join(expected, '\n')
        actual = readchomp(io)
        if isempty(expected)
            @test startswith(actual, "No matching tests for module")
        else
            @test actual == expected
        end
    end
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

# by default, respect order of tests:
check("", ["a", "b1", "b2", "c", "d", "e1", "e2", "f1", "g", "h1", "h2"])
retest(M)

module N
using ReTest

RUN = []

# testing non-final non-toplevel testsets
@testset "i" begin
    push!(RUN, "i")
    @test true

    @testset "j" verbose=false begin # false, just to test that it works
        push!(RUN, "j")
        @test true

        @testset "k" begin
            push!(RUN, "k")
            @test true
        end
    end

    @testset "l$i" verbose=true for i=1:1
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
    retest(N, Regex(rx))
    @test sort(RUN) == sort(list)
    empty!(RUN)
    N.runtests(Regex(rx))
    @test sort(RUN) == sort(list)
end
end

import .N
N.check(".*j1", [])
N.check(".*j/1", [])
N.check("^/i/j0", [])
N.check("^/i/l10", [])

### module P #################################################################

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

@testset "D&E" begin
    push!(RUN, "D&E")
    @test true
end

function check(rx, list)
    empty!(RUN)
    retest(P, rx, verbose=0)
    @test sort(RUN) == sort(list)
    empty!(RUN)
    P.runtests(rx, verbose=2)
    @test sort(RUN) == sort(list)
end
end

import .P # test ReTest's wrapping of non-regex patterns
P.check("b", ["a", "b", "b|c"]) # an implicit prefix r".*" is added
P.check("B", ["a", "b", "b|c"]) # idem, case-insensitive
P.check(r"B", []) # not case-insensitive

if VERSION >= v"1.3"
    P.check("b|c", ["a", "b|c"]) # "b" is not matched
    P.check("B|C", ["a", "b|c"]) # idem, case-insensitive
end

P.check("d&e", ["D&E"])
P.check("d&E", ["D&E"])
P.check(r"d&E", [])
P.check(r"d&E"i, ["D&E"])


### toplevel #################################################################

# The following test is just to exert `@assert allunique(TESTED_MODULES)` in
# computemodules!, and must be run before any toplevel @testset in declared,
# so that `Main` is not yet in TESTED_MODULES; we check that previous
# tested modules, which are in submodules of Main, are not re-added to
# TESTED_MODULES while going through submodules of Base.loaded_modules ∋ Main
retest(dry=true)

RUN = []
@testset "toplevel" begin
    # this tests that the testset is run exactly once
    # Main is special here, as it's both in Base.loaded_modules
    # and it gets registered automatically in ReTest.TESTED_MODULES
    push!(RUN, "toplevel")
    @test true
end

retest(verbose=Inf)
retest("a", shuffle=true, stats=true)
retest(M, N, P, "b", dry=true)
@test_throws ArgumentError retest("A", r"b")

for v in (-rand(1:9), 4.2, -Inf)
    @test_throws ArgumentError retest(verbose=v)
end

@test RUN == ["toplevel"]

retest(r"^/f1", stats=true) # just test that a regex can be passed,
                            # and that stats works for multiple subtests

empty!(RUN)

module Overwritten
using ReTest
import Main: RUN
@testset "first" begin
    push!(RUN, 1)
end
end
retest(Overwritten, stats=true) # testing stats when there are not tests
@test RUN == [1]

empty!(RUN)

module Overwritten
using ReTest
import Main: RUN
@testset "second" begin
    push!(RUN, 2)
end
end
retest(Overwritten)
@test RUN == [2] # 1 must not appear (i.e. ReTest must not keep a reference to
                 # the old overwritten version of `Overwritten`

### Loops ####################################################################

module Loops1
using ReTest

RUN = []

@testset "loops 1" begin
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

@test_logs (:warn, r"could not evaluate testset-for iterator.*") retest(Loops1, r"asd")
@test Loops1.RUN == [1, 0, 2, 0]
empty!(Loops1.RUN)
retest(Loops1) # should not log
@test Loops1.RUN == [1, 0, -1, 2, 0, -1]

module Loops2
using ReTest

RUN = []

@testset "loops 2" begin
    @testset "generator $i $I" for (i, I) in (i => typeof(i) for i in (1, 2))
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

# the test below has been solved
# TODO: check whether another run could lead to the same result, i.e. RUN == [1, 0, 2, 0] ?
# @test_logs (:warn, r"could not evaluate testset-for iterator.*") retest(Loops2, r"asd")
# @test Loops2.RUN == [1, 0, 2, 0]
# empty!(Loops2.RUN)
@test isempty(Loops2.RUN)

retest(Loops2)
@test Loops2.RUN == [1, 0, -1, 2, 0, -1]

module Loops3
using ReTest

RUN = []

@testset "loops 3" begin
    @testset "generator $i $I" for (i, I) in [i => typeof(i) for i in (1, 2)]
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

retest(Loops3, r"asd")
@test Loops3.RUN == []
empty!(Loops3.RUN)
retest(Loops3)
@test Loops3.RUN == [1, 0, -1, 2, 0, -1]

module MultiLoops
using ReTest

RUN = []
C1, C2 = 1:2 # check that iteration has access to these values

@testset "multiloops $x $y $z" for (x, y) in zip(1:C2, 1:2), z in C1:x
    push!(RUN, (x, z))
    @test true
end
end

retest(MultiLoops)
@test MultiLoops.RUN == [(1, 1), (2, 1), (2, 2)]
empty!(MultiLoops.RUN)
retest(MultiLoops, "1 1")
@test MultiLoops.RUN == [(1, 1)]

### Failing ##################################################################

module Failing
using ReTest

@testset "has fails" begin
    @test false
end
end

@test_throws Test.TestSetException retest(Failing)

### Duplicate ################################################################

module Duplicate
using ReTest

RUN = []

@testset "dupe" begin
    @test true
    push!(RUN, 1)
end
@testset "dupe" begin
    @test true
    push!(RUN, 2)
end

@testset "dupe$i" for i=3:4
    @test true
    push!(RUN, i)
end
@testset "dupe$i" for i=5:6
    @test true
    push!(RUN, i)
end
end

@test_logs (
    :warn, r"duplicate description for @testset, overwriting:.*") (
    :warn,  r"duplicate description for @testset, overwriting:.*")  Duplicate.runtests()
@test Duplicate.RUN == [2, 5, 6]

### Uniquify #################################################################

empty!(Duplicate.RUN)

retest(Duplicate, Duplicate, "dupe5")
@test Duplicate.RUN == [5]

### Display ##################################################################

# we exercise a bunch of codepaths, and this allows to have an idea whether
# everything prints properly (writing proper display tests will be soooo boring)

module X
using ReTest

@testset "X.full" begin
    @test true
    @testset "X.inner" begin
    end
end
end

module YYYYYYYYYYYYY
using ReTest

@testset "Y.broken" begin
    @testset "hidden" begin
        # we "hide" it to be sure that the .hasbrokenrec field is used and not .hasbroken
        @test_broken false
    end
end
@testset "Y.loop $('*'^(10-3*i))" for i=1:2
    @test true
    @testset "Y.inner_____________" begin
        @test true
        @testset "Y.core" begin
            @test true
        end
    end
end
end

module NoTests
using ReTest

@testset "empty" begin end
@testset "empty $i" for i=1:1 end
end

for dry=(true, false),
    verbose=0:3,
    stats=(true, false),
    mod=((X,), (YYYYYYYYYYYYY,), (X, YYYYYYYYYYYYY), (NoTests,)),
    regex=(r"inner", r"", "not matching")

    if regex == "not matching"
        mod in [(X,), (X, YYYYYYYYYYYYY)] || continue
        verbose in 0:1 || continue
    end
    if mod == (NoTests,)
        dry == false && verbose in 0:1 && regex == r"" ||
            continue
    end
    modstr = length(mod) == 1 ? string(mod[1]) : string(mod)
    println("\n####################### Display: mod=$modstr stats=$stats dry=$dry verbose=$verbose regex=$regex\n")
    retest(mod..., regex; shuffle=true, verbose=verbose, stats=stats, dry=dry)
end

### InlineTest ###############################################################

using Pkg
Pkg.activate("./FakePackage")
Pkg.develop(PackageSpec(path="../InlineTest"))
Pkg.develop(PackageSpec(path="..")) # ReTest
Pkg.test("FakePackage")
