using Pkg
import Test

Pkg.activate("ReTest")
Pkg.develop(PackageSpec(path="../InlineTest"))

using ReTest # must be after develop of InlineTest, so as to not load a registered version
include("setup.jl")

include("test_patterns.jl")

# * M ........................................................................

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

function check(rx, list; implicit=false)
    empty!(RUN)
    if implicit
        retest(Regex(rx))
    else
        retest(M, Regex(rx))
    end
    @test RUN == list
    mktemp() do path, io
        redirect_stdout(io) do
            if implicit
                retest(Regex(rx), dry=true, id=false)
            else
                retest(M, Regex(rx), dry=true, id=false) # TODO: test with id=true
            end
        end
        seekstart(io)
        expected = map(list) do t
            "  "^implicit *
                if t in innertestsets
                    "  " * t * " ✔"
                else
                    t * " ✔"
                end
        end
        expected = join(expected, '\n')
        if implicit
            expected = "Main.M\n" * expected
        end
        actual = readchomp(io)
        if isempty(expected)
            @test startswith(actual, "No matching tests for module")
        else
            @test actual == expected
        end
    end
end
end #

@chapter OnlyOneModule begin
    M.check("a", ["a"], implicit=true)
end

@chapter M begin
    # we don't put these checks in a @testset, as this would modify filtering logic
    # (e.g. with `@testset "filtering" ...`, testset subjects would start with "/filtering/...")
    M.check("a", ["a"])
    M.check("a1", []) # testset is "final", so we do a full match
    M.check("b", ["b1", "b2"])
    M.check("/b", ["b1", "b2"])
    M.check("b1", ["b1"])
    M.check("c1", []) # "c1" is *not* partially matched against "/c/"
    M.check("c/1", []) # "c" is partially matched, but nothing fully matches afterwards
    M.check("c/d1", [])
    M.check("c/d", ["c", "d"])
    M.check("/c", ["c", "d", "e1", "e2"])
    M.check(".*d", ["c", "d"])
    M.check(".*(e1|e2)", ["c", "e1", "e2"])
    M.check("f1", ["f1", "g", "h1", "h2"])
    M.check("f",  ["f1", "g", "h1", "h2"])
    M.check(".*g", ["f1", "g"])
    M.check(".*h", ["f1", "h1", "h2"])
    M.check(".*h1", ["f1", "h1"])
    M.check(".*h\$", [])

    # by default, respect order of tests:
    M.check("", ["a", "b1", "b2", "c", "d", "e1", "e2", "f1", "g", "h1", "h2"])
    retest(M)
end


# * N ........................................................................

module N
using ReTest, ..Trace

# testing non-final non-toplevel testsets
@testset "i" begin
    trace("i")

    @testset "j" verbose=false begin # false, just to test that it works
        trace("j")

        @testset "k" begin
            trace("k")
        end
    end

    @testset "l$i" verbose=true for i=1:1
        trace("l$i")

        @testset "m" begin
            trace("m")
        end
    end
end
end # N

@chapter N begin
    check(N, r".*j1", ""; runtests=true)
    check(N, r".*j/1", ""; runtests=false)
    check(N, r"^/i/j0", ""; runtests=true)
    check(N, r"^/i/l10", ""; runtests=false)

    # strict keyword
    check(N, 1, "i")
    check(N, 1, "i", strict=true)
    check(N, 1, "i j l1", strict=false)

    # reachable
    if VERSION >= v"1.3"
        check(N, reachable(1), " i j k l1 m")
        check(N, reachable(1), dry=true, id=false, verbose=3, "", output = """
i
  j
    k
  l1
    m
""")
        check(N, reachable("l1"), " i l1 m")
        check(N, not(reachable("l1")), " i j k")
    end
end

# * P ........................................................................
# test ReTest's wrapping of non-regex patterns

module P
using ReTest, ..Trace

@testset "a" begin
    trace("a")

    @testset "b" begin
        trace("b")
    end

    @testset "b|c" begin
        trace("b|c")
    end
end

@testset "D&E" begin
    trace("D&E")
end
end # P

@chapter P begin
    check(P, "b", "a b b|c"; verbose=2) # an implicit prefix r".*" is added
    check(P, "B", "a b b|c"; verbose=0, runtests=true) # idem, case-insensitive
    check(P, r"B", "") # not case-insensitive

    if VERSION >= v"1.3"
        check(P, "b|c", "a b|c") # "b" is not matched
        check(P, "B|C", "a b|c") # idem, case-insensitive
    end

    check(P, "d&e", "D&E")
    check(P, "d&E", "D&E")
    check(P, r"d&E", "")
    check(P, r"d&E"i, "D&E")
end


# * Depth

module Depth
using ReTest, ..Trace
@testset "1" begin
    trace(1)
    @testset "2" begin
        trace(2)
        @testset "3" begin
            trace(3)
        end
    end
    @testset "4" begin
        trace(4)
    end
end
end

@chapter Depth begin
    check(Depth, depth(2), [1, 2, 4])
    check(Depth, depth(3), [1, 2, 3])
    check(Depth, depth.(2:3), [1, 2, 3, 4])
    if VERSION >= v"1.3"
        check(Depth, reachable(depth(2)), [1, 2, 3, 4])
    end
end


# * retest can be called on parent modules

module Parent
module Child
using ReTest, ...Trace

@testset "child" begin
    trace("child")
end
end # Child
end # Parent

@chapter Parent begin
    check(Parent, "child", runtests=true)
end

# * multiple patterns ........................................................

module MultiPat
using ReTest, ..Trace

@testset "a" begin
    trace("a")

    @testset "b" begin
        trace("b")
    end
end

@testset "aa" begin
    trace("aa")
end

@testset "b" begin
    trace("c")
end

@testset "d$i" for i=1:2
    trace("d$i")
end

end # MultiPat

@chapter MultiPat begin
    check(MultiPat, "a", "a b aa")
    check(MultiPat, "b", "a b c")
    check(MultiPat, "a", "b", "a b")
    check(MultiPat, ("a", "b"), "a b")
    check(MultiPat, "b", r"a", "a b")
    check(MultiPat, "b", "d", "")
    check(MultiPat, "a", "e", "")
    check(MultiPat, ["aa", "2"], "aa d2")
    check(MultiPat, ["1", "2"], "d1 d2")
    check(MultiPat, ["aa", "2"], "d", "d2")
    check(MultiPat, ["aa", "2", ["2", "1"]], "d", "d1 d2")

    # with integers
    # check we don't collect the range:
    check(MultiPat, 1:Int64(10)^15, "a b aa c d1 d2")
    check(MultiPat, 5, "d1 d2")
    check(MultiPat, "a", 2:3, "a b aa")
    check(MultiPat, "a", 1:2, "a b")
    check(MultiPat, "a", [1, 3], "a aa")
    check(MultiPat, ["aa", 4], "aa c")

    # with Not
    check(MultiPat, not("a"), "c d1 d2")
    check(MultiPat, not(4:10), "a b aa")
    check(MultiPat, not((1:3, "b")), "a aa c d1 d2")
    check(MultiPat, not([1:2, "aa", not("d")]), "d1 d2")
    check(MultiPat, -2, -4, -5, -6, "a aa")
    check(MultiPat, (-3, "a"), "a b")
    check(MultiPat, -1:3, "a b aa c d1 d2") # TODO: test this were 2, 3 can be tested
                                            # without forcing also testing 1
end


# * StringPat ................................................................

module StringPat
using ReTest, ..Trace

@testset "aa" begin trace("aa") end
@testset "-a" begin trace("-a") end
@testset "b" begin trace("b") end

end # StringPat

@chapter StringPat begin
    check(StringPat, "a", "aa -a")
    check(StringPat, "-a", "b")
    check(StringPat, "--a", "-a")
    check(StringPat, not("--a"), "aa b")
    check(StringPat, "-aa", "-a b")
end


# * Module patterns ..........................................................

module ModPat
using ReTest, ..Trace

@testset "aa" begin
    trace("aa")
    @testset "b" begin
        trace("b")
    end
end

@testset "ab" begin
    trace("ab")
end

module In
module Ner
using ReTest, ....Trace

@testset "c" begin
    trace("c")
end
end end # In.Ner
end # ModPat

module ModPat2
using ReTest, ..Trace

@testset "ad" begin
    trace("ad")
end
end # ModPat2

@chapter ModPat begin
    check(ModPat, "aa b ab c")
    check(ModPat => 1:99, "aa b ab c") # recursive
    check(ModPat => 1:99, "aa b ab", recursive=false) # no recursive
    check(ModPat => "a", 1:2, "aa b")
    check(ModPat => "a", ModPat => 1:2, "aa b") # patterns are merged for a module
    check(ModPat => 3:3, ModPat.In => "c", "ab") # In inherits from ModPat
    check(3:3, ModPat.In.Ner => "c", "") # In.Ner inherits from global patterns
    check(ModPat2 => "a", ModPat, 1:9, "ad aa b ab c") # ModPat recursive
    check(ModPat2 => ("a", 1:9), ModPat => 1:9, "ad aa b ab c") # ModPat recursive
    check(ModPat2 => ("a", 1:9), ModPat => 1:9, "ad aa b ab",
          recursive=false) # ModPat not recursive
    check(ModPat2 => "a", ModPat, 2:9, "aa b ab")
    check(ModPat2 => "a", ModPat, ModPat, 2:9, "aa b ab") # deduplicate
    check(ModPat2 => "", ModPat => "aa", 2, "aa b")
end


# * toplevel .................................................................

@chapter toplevel false begin
    # The following test is just to exert `@assert allunique(TESTED_MODULES)` in
    # computemodules!, and must be run before any toplevel @testset in declared,
    # so that `Main` is not yet in TESTED_MODULES; we check that previous
    # tested modules, which are in submodules of Main, are not re-added to
    # TESTED_MODULES while going through submodules of Base.loaded_modules ∋ Main
    retest(dry=true)
    RUNTOP = []

    @testset "toplevel" begin
        # this tests that the testset is run exactly once
        # Main is special here, as it's both in Base.loaded_modules
        # and it gets registered automatically in ReTest.TESTED_MODULES
        push!(RUNTOP, "toplevel")
        @test true
    end

    retest(verbose=Inf)
    retest("a", shuffle=true, stats=true)
    retest(M, N, P, "b", dry=true)

    for v in (-rand(1:9), 4.2, -Inf)
        @test_throws ArgumentError retest(verbose=v)
    end

    @test RUNTOP == ["toplevel"]

    retest(r"^/f1", stats=true) # just test that a regex can be passed,
                                # and that stats works for multiple subtests
end


# * Overwritten ..............................................................

module Overwritten
using ReTest, ..Trace

@testset "overwritten first" begin
    trace(1)
end
end # Overwritten

@chapter Overwritten begin
    check(Overwritten, [1]; stats=true) # testing stats when there are not tests
end

module Overwritten
using ReTest, ..Trace

@testset "overwritten second" begin
    trace(2)
end
end # Overwritten

@chapter Overwritten begin
    check(Overwritten, [2])
    # 1 must not appear, i.e. ReTest must not keep a reference to
    # the old overwritten version of `Overwritten`
end

module Overwritten
# here Overwritten also doen't define a @testset, ony its submodule does
# we want to check that the old version of Overwritten gets detected
# as "replaced" and deleted from TESTED_MODULES

module Sub
using ReTest, ...Trace

@testset "overwritten second" begin
    trace(3)
end
end
end

@chapter Overwritten begin
    check("overwritten ", [3])
    # 1 must not appear, i.e. ReTest must not keep a reference to
    # the old overwritten version of `Overwritten`, even as Overwritten is not
    # anymore in TESTED_MODULES
end

module Overwritten end # no testset at all

@chapter Overwritten begin
    check("overwritten", []) # all previous modules/submodules are "replaced"
end


# * Loops ....................................................................

module Loops1
using ReTest, ..Trace

@testset "loops 1" begin
    trace(9)
    a = 1
    b = 2

    @testset "local$i" for i in (a, b)
        trace(i)
        @testset "sub" begin
            trace(0)

            @testset "final" begin
                trace(-1)
            end
        end
    end
end
end # Loops1

@chapter Loops begin
    # @test_logs (:warn, r"could not evaluate testset description.*") retest(Loops1, r"asd")
    # @test Loops1.RUN == [1, 0, 2, 0]
    # empty!(Loops1.RUN)
    # retest(Loops1) # should not log
    check(Loops1, [9, 1, 0, -1, 2, 0, -1])
    check(Loops1, 9:9, []) # when no regex is passed, even with the presence of statically
                           # unresolvable descriptions, we can filter out stuff
                           # (i.e. here, we don't run "loops 1" just in case "local$i" would
                           # be run, as we can determine from pattern 9:9 that nothing must run)
    check(Loops1, interpolated, "",  dry=true, verbose=9, id=false, output = "loops 1")
    check(Loops1, not(interpolated), "",  dry=true, verbose=9, id=false, output = """
loops 1
  "local\$(i)" (repeated)
    sub
      final
""")
    if VERSION >= v"1.3"
        check(Loops1, reachable("loops 1"), verbose=9, [9, 1, 0, -1, 2, 0, -1])
        check(Loops1, reachable("loops 1"), verbose=9, dry=true, id=false, [], output=raw"""
loops 1
  "local$(i)" (repeated)
    sub
      final
""")
        check(Loops1, reachable("loops 1"), verbose=9, dry=true, id=false, static=true, [],
              output="loops 1")
        check(Loops1, reachable(interpolated), verbose=9, dry=true, id=false, [],
              output="loops 1")
    end
end

# same as Loops1, but the iterator can be statically computed, only descriptions can't
module Loops1bis
using ReTest, ..Trace

@testset "loops 1 bis" begin
    trace(9)
    a = 0
    @testset "local $a $i" for i in (1, 2)
        trace(i)
        @testset "sub" begin
            trace(0)

            @testset "final" begin
                trace(-1)
            end
        end
    end
end
end # Loops1bis

@chapter Loops begin
    check(Loops1bis, interpolated, "",  dry=true, verbose=9, id=false, output = "loops 1 bis")
    check(Loops1bis, not(interpolated), "",  dry=true, verbose=9, id=false, output = """
loops 1 bis
  "local \$(a) \$(i)" (repeated 2 times)
    sub
      final
""")
end

module Loops2
using ReTest, ..Trace

@testset "loops 2" begin
    @testset "generator $i $I" for (i, I) in (i => typeof(i) for i in (1, 2))
        trace(i)
        @testset "sub" begin
            trace(0)

            @testset "final" begin
                trace(-1)
            end
        end
    end
end
end # Loops2

# the test below has been solved
# TODO: check whether another run could lead to the same result, i.e. RUN == [1, 0, 2, 0] ?
# @test_logs (:warn, r"could not evaluate testset-for iterator.*") retest(Loops2, r"asd")
# @test Loops2.RUN == [1, 0, 2, 0]

@chapter Loops check(Loops2, [1, 0, -1, 2, 0, -1])

module Loops3
using ReTest, ..Trace

@testset "loops 3" begin
    @testset "generator $i $I" for (i, I) in [i => typeof(i) for i in (1, 2)]
        trace(i)
        @testset "sub" begin
            trace(0)

            @testset "final" begin
                trace(-1)
            end
        end
    end
end
end # Loops3

@chapter Loops begin
    check(Loops3, r"asd", [])
    check(Loops3, [1, 0, -1, 2, 0, -1])
end

module MultiLoops
using ReTest, ..Trace

C1, C2 = 1:2 # check that iteration has access to these values

@testset "multiloops $x $y $z" for (x, y) in zip(1:C2, 1:2), z in C1:x
    trace((x, z))
end
end # MultiLoops

@chapter Loops begin
    check(MultiLoops, [(1, 1), (2, 1), (2, 2)])
    check(MultiLoops, "1 1", [(1, 1)])
end

module LoopsVariablesDryrun
# check that even with for-iterators which depend on previous loop variables,
# dryrun mode is able to compute them and corresponding descriptions,
# and filter accordingly

using ReTest

@testset "a$i" for i=1:2
    @testset "b$j" for j=1:i
        @test true
    end
end
end

@chapter Loops begin
    # no match, so this it at least filtered for final testsets
    check(LoopsVariablesDryrun, "a1/b3", dry=true, verbose=9, [], output="""
1| a1
1| a2
""")
    check(LoopsVariablesDryrun, "a2/b1", dry=true, verbose=9, [], output="""
1| a1
1| a2
2|   b1
""")
end

# * Anonym ...................................................................

module Anonym
using ReTest, ..Trace

@testset for x=1:2
    @testset begin
        trace(x)
    end
end
end # Anonym

@chapter Anonym check(Anonym, [1, 2])


# * loop variable collision ..................................................

module LoopCollision
using ReTest, ..Trace

@assert isdefined(LoopCollision, :sincos)
@assert isdefined(LoopCollision, :sinpi)

@testset "collision $sincos" for sincos in (sin, cos)
    @test sincos(1.0) isa Float64
    trace(sincos)
end

@testset "collision $sincos sinpi $sinpi" for sincos = (sin, cos), sinpi = (1,)
    trace((sincos, sinpi))
end

end # LoopCollision

@chapter LoopCollision begin
    retest(LoopCollision, dry=true)
    check(LoopCollision, [sin, cos, (sin, 1), (cos, 1)]; dry=false)
end


# * interpolated description .................................................

module Interpolate
using ReTest, ..Trace

X = 0

@testset "a $X" verbose=true begin
    @testset "b $X" begin
        trace(1)
    end
    @testset "c $X $i" for i=2:3
        trace(i)
    end
end

@testset "d $X $i" verbose=true for i=4:4
    trace(i)
    @testset "e $X" begin
        trace(5)
    end
end
end # Interpolate

@chapter Interpolate begin
    retest(Interpolate, dry=true)
    check(Interpolate, 1:5)
    check(Interpolate, "0", 1:5)
    check(Interpolate, "4", 4:5)
    check(Interpolate, interpolated, 1:5)
    check(Interpolate, not(interpolated), [])
    check(Interpolate, static=false, [])
end

module InterpolateImpossible
using ReTest, ..Trace

X = 0

@testset "a $X" verbose=true begin
    trace(0)
    j = 9
    @testset "b $X $j" begin
        trace(1)
    end
    @testset "c $X $j $i" for i=2:3
        trace(i)
    end
end

@testset "d $X $i" verbose=true for i=4:4
    trace(i)
    @testset "e $X $i" begin
        trace(5)
    end
    @testset "$i" begin # must work even "$i" is made only of Expr (no String parts)
        trace(6)
    end
end
end # InterpolateImpossible

@chapter Interpolate begin
    retest(InterpolateImpossible, dry=true)
    check(InterpolateImpossible, 0:6)
    check(InterpolateImpossible, interpolated, [0, 4])
    check(InterpolateImpossible, "0", 0:6)
    check(InterpolateImpossible, "4", [0, 4, 5, 6]) # should have a warning or two
    check(InterpolateImpossible, "d", not(2:3), 4:6)
    check(InterpolateImpossible, "d", [0, 4, 5, 6])
    check(InterpolateImpossible, "d", interpolated, [4])
    check(InterpolateImpossible, "d", not(interpolated), [0, 4])
    check(InterpolateImpossible, static=nothing, 0:6)
    check(InterpolateImpossible, static=true, 0:6)
    check(InterpolateImpossible, static=false, [])
    check(InterpolateImpossible, r".*", static=nothing, 0:6)
    check(InterpolateImpossible, r".*", static=true, [0, 4])
    check(InterpolateImpossible, r".*", static=false, 0:6)
    # "" pattern is recognized as match-all
    check(InterpolateImpossible, "", static=nothing, 0:6)
    check(InterpolateImpossible, "", static=true, 0:6)
    check(InterpolateImpossible, "", static=false, [])
end

# * Misc .....................................................................

module MiscDuplicity # a testset is both "match" and "undecidable"
using ReTest, ..Trace

@testset "a" begin
    x = 2
    @testset "b$(i==1 ? 1 : x)" for i=1:2
        @testset "c" begin
            # subject is undecidable at second iteration, ts.run must not be
            # overwritten at the 2nd iteration, but be the "or" `|` of each
            # iteration decision
            trace(i)
            @test true
        end
    end
end
end # MiscDuplicity

@chapter MiscDuplicity begin
    check(MiscDuplicity, "c", static=true, 1:2)
    check(MiscDuplicity, "c", static=false, 1:2)
    check(MiscDuplicity, "b", static=true, 1:2)
    check(MiscDuplicity, "b", static=false, 1:2)
    check(MiscDuplicity, "c", verbose=3, dry=true, id=false, [], output= """
a
  b1
    c
  "b?" (repeated 1 times)
    c
""")

    # in dryrun mode, isfinal testsets might have to be filtered out even when
    # their description can't be interpolated (finding such an example was initial
    # motivation to add the iter(n)::Iter pattern)
    check(MiscDuplicity, (2, iter(1)), dry=true, id=false, verbose=2, [], output="""
a
  b1
""")
    check(MiscDuplicity, (2, iter(2)), dry=true, id=false, verbose=2, [], output="""
a
  "b?" (repeated 1 times)
""")
end

module MV # MiscVerbose, but we want the name to be short to not have effect on alignment
using ReTest

@testset "outer" verbose=true begin # verbose must not be lost when computing alignment
    @test true
    inner = "inner"
    @testset "$inner" begin
        @test true
    end
end
end # MV

@chapter MiscVerbose begin
    check(MV, [], output = """
             Pass
outer    |      2
  inner  |      1
""")
end

module MiscSeed
using ReTest
RAND1 = nothing
RAND2 = nothing

rand(UInt16)

@testset "rand1" begin
    global RAND1 = rand(UInt16)
end

@testset "rand2" begin
    global RAND2 = rand(UInt16)
end

end

@chapter MiscSeed begin
    rands = VERSION < v"1.7-" ? [0x24ae, 0x837e] :
                                [0x1f33, 0x415f]
    MiscSeed.runtests(verbose=0, seed=1)
    @test MiscSeed.RAND1 === MiscSeed.RAND2 === rands[1]
    MiscSeed.runtests(verbose=0, seed=2)
    @test MiscSeed.RAND1 === MiscSeed.RAND2 === rands[2]
    MiscSeed.runtests(verbose=0) # no seeding
    @test MiscSeed.RAND1 === MiscSeed.RAND2 === rands[2]
    MiscSeed.runtests(verbose=0, seed=false) # no seeding
    @test MiscSeed.RAND1 === MiscSeed.RAND2 === rands[2]

    rand1 = MiscSeed.RAND1
    MiscSeed.runtests(verbose=0, seed=true) # random seeding
    @test MiscSeed.RAND1 === MiscSeed.RAND2
    rand2 = MiscSeed.RAND1
    MiscSeed.runtests(verbose=0, seed=true) # random seeding
    @test MiscSeed.RAND1 === MiscSeed.RAND2
    rand3 = MiscSeed.RAND1
    @test rand1 != rand2 || rand1 != rand3 # || to reduce failure rate
    # TODO: test in a distributed setting
end

module MiscSubmoduleHeader
module Sub
using ReTest
@testset "a" begin end
end
end

@chapter MiscSubmoduleHeader begin
    check(MiscSubmoduleHeader, recursive=true, dry=true, [], output="""
Main.MiscSubmoduleHeader.Sub
1| a
""")
    check(MiscSubmoduleHeader.Sub, recursive=true, dry=true, [], output="""
1| a
""")
end


module MiscIsFinal
using ReTest, ..Trace

@testset "outer$i" for i=1:2
# "outer$i" is apparently non-final, but with the proper filtering pattern,
# it can become final: then, test that filtering actually happens
# (once `resolve!` is done, no more filtering happens except for `isfinal` testsets;
# so here we check that `isfinal` applies to "outer")
    trace(i)
    @testset "inner" begin
        trace(-1)
    end
end

end # MiscIsFinal

@chapter MiscIsFinal begin
    check(MiscIsFinal, r"outer1$", [1])
    check(MiscIsFinal, r"outer1$", dry=true, id=false, [], output="outer1")
    check(MiscIsFinal, r"outer2$", [2])
    check(MiscIsFinal, r"outer2$", dry=true, id=false, [], output="outer2")
end


module Bugs
using ReTest
@testset "macros with nothing" begin
    # in parse_ts(), we were returning `nothing` to mean invalid testset,
    # and then filtering out those; but some `Expr`s have meaningful
    # `nothing` literals which must *not* be filtered out, e.g.
    # the `@int128_str` macro:
    @test 24_061_467_864_032_622_473_692_149_727_991 isa Integer
end
end # Bugs

@chapter Bugs begin
    check(Bugs, [])
end


module TestsetErrors
using ReTest

@testset
@testset "a" notexistingoption=0 begin end
@testset "b" {badsyntax} begin end
@testset "c"
@testset "d" let; end
end

@chapter TestsetErrors begin
    @test_logs (
        :error, "expected begin/end block or for loop as argument to @testset") (
        :error, "unsupported @testset option") (
        :error, "unsupported @testset" ) (
        :error, "expected begin/end block or for loop as argument to @testset") (
        :error, "expected begin/end block or for loop as argument to @testset"
        ) TestsetErrors.runtests()
end


# * Marks ....................................................................

module Marks
using ReTest, ..Trace

@testset "a" begin
    trace("a")
    @testset "b" begin end # no test, counts as pass
    @testset "c" begin
        trace("c")
    end
    x = 1
    @testset "d$x" begin
        trace("d$x")
    end
    @testset "e" begin
        @test false
    end
end

@testset "l$i" for i=1:2
    @testset "k$j" for j=1:2
        @test true
    end
end

@testset "x" :hx begin
    @testset "y$i" for i=1:2
        @testset :hz1 "z1" begin
            @test true
        end
        @testset "z$j" for j=2:3
            @test true
        end
        @testset "z4" begin
            @test i == 1
        end
    end
end
end # Marks

@chapter Marks begin
    check(Marks, "-x", dry=true, marks=true, verbose=3, id=false, [], output=raw"""
a
  b
  c
  "d$(x)"
  e
l1
  k1
  k2
l2
  k1
  k2
""")
    check(Marks, "-x", dry=true, marks=true, verbose=1, id=false, [], output=raw"""
a ⋯
l1 ⋯
l2 ⋯
""")
    retest(Marks, "-x", "k1")
    check(Marks, "-x", dry=true, marks=true, verbose=3, id=false, [], output=raw"""
a ✔
  b
  c
  "d$(x)"
  e
l1 ✔
  k1 ✔
  k2
l2 ✔
  k1 ✔
  k2
""")
    check(Marks, "-x", dry=true, marks=true, verbose=1, id=false, [], output=raw"""
a ✔ ⋯
l1 ✔ ✔ ⋯
l2 ✔ ✔ ⋯
""")
    @test_throws Test.TestSetException retest(Marks, "e")
    check(Marks, "-x", "a", dry=true, marks=true, verbose=3, id=false, [], output=raw"""
a ✔
  b
  c
  "d$(x)"
  e ✘
""")
    check(Marks, "-x", "a", dry=true, marks=true, verbose=1, id=false, [], output=raw"""
a ✔ ✘ ⋯
""")
    retest(Marks, "b")
    check(Marks, "a", dry=true, marks=true, verbose=3, id=false, [], output=raw"""
a ✔
  b ✔
  c
  "d$(x)"
  e ✘
""")
    check(Marks, "a", dry=true, marks=true, verbose=1, id=false, [], output=raw"""
a ✔ ✔ ✘ ⋯
""")
    retest(Marks, "l2")
    check(Marks, "-x", dry=true, marks=true, verbose=3, id=false, [], output=raw"""
a ✔
  b ✔
  c
  "d$(x)"
  e ✘
l1 ✔
  k1 ✔
  k2
l2 ✔
  k1 ✔
  k2 ✔
""")
    check(Marks, "-x", dry=true, marks=true, verbose=1, id=false, [], output=raw"""
a ✔ ✔ ✘ ⋯
l1 ✔ ✔ ⋯
l2 ✔ ✔
""")

    check(Marks, "x", dry=true, marks=true, verbose=3, id=false, interpolated, [], output="""
x hx
  y1 hx
    z1 hx hz1
    z2 hx
    z3 hx
    z4 hx
  y2 hx
    z1 hx hz1
    z2 hx
    z3 hx
    z4 hx
""")
    check(Marks, "x", dry=true, marks=true, verbose=2, id=false, interpolated, [], output="""
x hx
  y1 hx ⋯
  y2 hx ⋯
""")
    retest(Marks, r"y1$")
    # at record success at depth==2
    check(Marks, "x", dry=true, marks=true, verbose=1, id=false, interpolated, [],
          output="x hx ✔ ✔ ⋯")
    @test_throws Test.TestSetException retest(Marks, "y2/z4")
    check(Marks, "x", dry=true, marks=true, verbose=1, id=false, interpolated, [],
          output="x hx ✔ ✔ ✘ ⋯")
    @test_throws Test.TestSetException retest(Marks, "x")
    check(Marks, "x", dry=true, marks=true, verbose=3, id=false, interpolated, [], output="""
x hx ✔
  y1 hx ✔
    z1 hx hz1 ✔
    z2 hx ✔
    z3 hx ✔
    z4 hx ✔
  y2 hx ✔
    z1 hx hz1 ✔
    z2 hx ✔
    z3 hx ✔
    z4 hx ✘
""")
    check(Marks, "x", dry=true, marks=true, verbose=2, id=false, interpolated, [], output="""
x hx ✔
  y1 hx ✔ ✔
  y2 hx ✔ ✔ ✘
""")
    check(Marks, "x", dry=true, marks=true, verbose=1, id=false, interpolated, [], output="""
x hx ✔ ✔ ✘
""")

    check(Marks, dry=true, clear=true, marks=true, id=false, [], output="""
a ✔ ✔ ✘ ⋯
l1 ✔ ✔ ⋯
l2 ✔ ✔
x hx ✔ ✔ ✘
""")
    check(Marks, dry=true, clear=true, marks=true, id=false, [], output="""
a ⋯
l1 ⋯
l2 ⋯
x hx ⋯
""")

    retest(Marks, "y1", -4, dry=true, tag=:ylabel) # should not label "/x"
    retest(Marks, r"a$", dry=true, tag=[:a1, :a2])
    retest(Marks, r"a$", dry=false)
    retest(Marks, r"a$", dry=true, tag=[:a3 :a4])
    retest(Marks, r"a$", dry=true, tag=(:a5, :a6))
    @test_throws ArgumentError retest(Marks, r"a$", dry=true, tag=:_underscored)
    @test_throws ArgumentError retest(Marks, r"a$", dry=true, tag=[not(:_underscored)])

    check(Marks, "-l", -4, dry=true, verbose=9, id=false, marks=true, [], output="""
a a1 a2 a3 a4 a5 a6 ✔
  b
  c
  e
x hx
  y1 hx ylabel
    z1 hx hz1 ylabel
    z2 hx ylabel
    z3 hx ylabel
    z4 hx ylabel
  y2 hx
    z1 hx hz1
    z2 hx
    z3 hx
    z4 hx
""")
    check(Marks, "a", "-l", -4, :a2, dry=true, verbose=9, id=false, marks=true, [],
          output="a a1 a2 a3 a4 a5 a6 ✔")
    check(Marks, "a", "-l", -4, :a2, dry=false, verbose=9, id=false, marks=true, ["a"])
    if VERSION >= v"1.3"
        check(Marks, "-l", -4, not(reachable(:a1)), dry=true, verbose=9, id=false,
              marks=true, [], output="""
x hx
  y1 hx ylabel
    z1 hx hz1 ylabel
    z2 hx ylabel
    z3 hx ylabel
    z4 hx ylabel
  y2 hx
    z1 hx hz1
    z2 hx
    z3 hx
    z4 hx
""")
        check(Marks, "-l", -4, reachable("x"), not(:ylabel), dry=true, verbose=9, id=false,
              marks=true, [], output="""
x hx
  y1 hx ylabel
  y2 hx
    z1 hx hz1
    z2 hx
    z3 hx
    z4 hx
""")
    end
    check(Marks, -4, [r"y1$", "z3"], dry=true, marks=true, id=false, tag=not(:ylabel),
          verbose=9, [], output="""
x hx
  y1 hx
    z3 hx
  y2 hx
    z3 hx
""")
    check(Marks, 8:12, dry=true, verbose=9, id=false, marks=true, [], output="""
x hx
  y1 hx
    z1 hx hz1 ylabel
    z2 hx ylabel
    z3 hx
    z4 hx ylabel
  y2 hx
    z1 hx hz1
    z2 hx
    z3 hx
    z4 hx
""")
    check(Marks, -4, :hx, not(:hz1), dry=true, verbose=9, id=false, marks=true, [], output="""
x hx
  y1 hx
    z2 hx ylabel
    z3 hx
    z4 hx ylabel
  y2 hx
    z2 hx
    z3 hx
    z4 hx
""")
    @test_logs (:warn, r"cannot remove statically attached label \(@testset :.* \.\.\.\)"
                ) retest(Marks, -4, :hx, not(:hz1), tag=not(:hx), dry=true)
end


# * Failing ..................................................................

module Failing
using ReTest

@testset "has fails" begin
    @test false
end
@testset "also has passes" begin
    @test true
end
@testset "unrun" begin
    @test true
end

end # Failing

@chapter Failing begin
    @test_throws Test.TestSetException retest(Failing)
    retest(Failing, "passes")

    check(Failing, dry=true, marks=true, -fail, [], output="""
2| also has passes ✔
3| unrun
""")
    check(Failing, dry=true, marks=true, pass, [], output="""
2| also has passes ✔
""")
    check(Failing, dry=true, marks=true, [pass, fail], [], output="""
1| has fails ✘
2| also has passes ✔
""")
    check(Failing, dry=true, marks=true, -pass, -fail, [], output="""
3| unrun
""")

    check(Failing, dry=true, marks=true, fail, [], output="1| has fails ✘")
    check(Failing, dry=true, marks=true, -pass, [], output="""
1| has fails ✘
3| unrun
""")
    retest(Failing, pass, -fail) # test that pass/fail filters are handled in non-dry mode
    check(Failing, dry=true, marks=true, [], output="""
1| has fails ✘
2| also has passes ✔
3| unrun
""")
end

module FailingLoops
# we test that toplevel testset-for don't make retest unresponsive

using ReTest

@testset "a$i" for i=1:3
    @test i == 1
end

@testset "b$i" for i=1:3
    @test i == 2
end

@testset "c$i" for i=1:3
    @test i == 3
end

end # FailingLoops

@chapter FailingLoops begin
    @test_throws Test.TestSetException retest(FailingLoops, "a")
    # TODO: we check here the behaviour by looking at check marks, we could maybe do better
    check(FailingLoops, "a", dry=true, marks=true, id=false, [], clear=true, output="""
a1 ✔
a2 ✘
a3
""")
    @test_throws Test.TestSetException retest(FailingLoops, "b")
    check(FailingLoops, "b", dry=true, marks=true, id=false, [], clear=true, output="""
b1 ✘
b2
b3
""")
    @test_throws Test.TestSetException retest(FailingLoops, "c")
    check(FailingLoops, "c", dry=true, marks=true, id=false, [], clear=true, output="""
c1 ✘
c2
c3
""")
end


# * Duplicate ................................................................

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
end # Duplicate

@chapter Duplicate begin
    @test_logs (
        :warn, r"duplicate description for @testset, overwriting:.*") (
        :warn,  r"duplicate description for @testset, overwriting:.*"
        )  Duplicate.runtests()
    @test Duplicate.RUN == [2, 5, 6]

    # uniquify
    empty!(Duplicate.RUN)
    retest(Duplicate, Duplicate, "dupe5")
    @test Duplicate.RUN == [5]

    # dup=true
    @eval Duplicate begin
        @testset "dupe" begin
            @test true
            push!(RUN, 7)
        end
    end
    empty!(Duplicate.RUN)
    retest(Duplicate, r"dupe$", dup=true)
    @test Duplicate.RUN == [2, 7]
end


# * Display ..................................................................

# we exercise a bunch of codepaths, and this allows to have an idea whether
# everything prints properly (writing proper display tests will be soooo boring)

module X
using ReTest

@testset "X.full" begin
    @test true
    @testset "X.inner" begin
    end
end
end # X

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
end # YYYYYYYYYYYYY

module NoTests
using ReTest

@testset "empty" begin end
@testset "empty $i" for i=1:1 end
end # NoTests

@chapter Display for dry=(true, false),
    verbose=0:3,
    stats=(true, false),
    mod=((X,), (YYYYYYYYYYYYY,), (X, YYYYYYYYYYYYY), (NoTests,)),
    pattern=(r"inner", 1:99, "not matching")

    id=rand([true, false, nothing])
    if pattern == "not matching"
        mod in [(X,), (X, YYYYYYYYYYYYY)] || continue
        verbose in 0:1 || continue
    end
    if mod == (NoTests,)
        dry == false && verbose in 0:1 && pattern == 1:99 ||
            continue
    end
    modstr = length(mod) == 1 ? string(mod[1]) : string(mod)
    # have to show 'X' below, because `nothing` can't be printed on old Julia
    println("\n####################### Display: mod=$modstr stats=$stats dry=$dry verbose=$verbose pattern=$pattern, id=$(something(id, 'X'))\n")
    retest(mod..., pattern; shuffle=true, verbose=verbose, stats=stats, dry=dry, id=id)
end


# * Alignment ................................................................

module Alignment
using ReTest, ..Trace

@testset "l$i" for i=1:2
end
end # Alignment

@chapter Alignment begin
    # here, there is only one testset, but it's a loop, make sure the alignment takes
    # into account that a "testset summary" will be printed
    check(Alignment, [], output="""
                   Pass
l1             |      0
l2             |      0
Main.Alignment |      0
""")
end


# * DryRun ...................................................................

module DryRun
using ReTest

X = 'a'

@testset "a" for i=1:2
    @testset "b" begin end
end
@testset "a$X" for i=1:2
    @testset "b" begin end
end
@testset "x$i" for i=1:2
    @testset "b$i" begin
        @testset "c" begin end
    end
    @testset "d$i$j" for j=1:2
    end
end
@testset "y$i" for i=1:1
    @testset "b" for j=1:i
        @testset "c" begin end
    end
end

end # DryRun

@chapter DryRun begin
    retest(DryRun, dry=true)
    retest(DryRun, dry=true, verbose=0)
    retest(DryRun, dry=true, verbose=5)
end

module DryRun2
using ReTest

@testset "just a dummy module" begin end
end # DryRun2

@chapter DryRun begin
    retest(DryRun, DryRun2, dry=true, verbose=0)
    retest(DryRun, DryRun2, dry=true, verbose=1)
end


# * @testset_macro ...........................................................


macro ts_macro(x)
    quote
        @testset "ts_macro" begin
            @test $x isa Int
        end
    end
end
@testset_macro @ts_macro
@testset "toplevel: ts_macro" begin
    @ts_macro 2
end

@chapter testset_macro begin
    check(Main, "ts_macro", [], id=false, dry=true, static=true, verbose=2, output = """
Main
  toplevel: ts_macro
    ts_macro
""")
end


# * InlineTest ...............................................................

using Pkg
# TODO: put the following 3 lines within chapters InlineTest and load, but for load,
# it should be enabled only if it was not already run from InlineTest
Pkg.activate("./FakePackage")
Pkg.develop(PackageSpec(path="../InlineTest"))
Pkg.develop(PackageSpec(path="..")) # ReTest

@chapter InlineTest false begin
    Pkg.test("FakePackage")
end

# * ReTest.load ..............................................................

using ReTest: process_args
module Load end
module Load2 end

@chapter load false begin
    using FakePackage
    @assert !isdefined(Main, :FakePackageTests)

    Test.@testset "ReTest.load" begin
        FT = ReTest.load(FakePackage, revise=false) # just stating the default for revise
        @test isdefined(Main, :FakePackageTests)
        @test FT === FakePackageTests
        @test first.(process_args((FakePackageTests,)).modules) ==
            [FakePackageTests, FakePackageTests.Sub, FakePackageTests.Sub.SubSub]

        ReTest.load(FakePackage, "FakePackageTests2.jl", parentmodule=Main)
        @test first.(process_args((AlternateFakePackageTests,)).modules) ==
            [AlternateFakePackageTests]
        @test_throws ErrorException ReTest.load(FakePackage, "notafile.jl")
        @test_throws ErrorException ReTest.load(Base)
        @test_throws ErrorException ReTest.load(Pkg)


        @test isempty(
            @test_logs (:warn,
                    r"test file .*load_noretest.jl loaded but it did not define any test module within Main.Load"
                    ) ReTest.load(FakePackage, "load_noretest.jl", parentmodule=Load))
        @test isempty(
            @test_logs (:warn,
                    "testsets were added directly into module $Load"
                    ) (:warn,
                       r"test file .*load_nomodule.jl loaded but it did not define any test module within Main.Load"
                       ) ReTest.load(FakePackage, "load_nomodule.jl", parentmodule=Load))

        @test isempty(
            @test_logs (:warn,
                    "test module Main.Load2.AltModule was defined but is not a (recursive) submodule of $Load"
                    ) (:warn,
                    r"test file .*load_altmodule.jl loaded but it did not define any test module within Main.Load"
                    ) ReTest.load(FakePackage, "load_altmodule.jl", parentmodule=Load))

        lpf = ReTest.load("Hijack/test/load_path.jl")
        @test Main.load_path_function() == 1
        @test lpf === Main.load_path_function
        @test_throws ErrorException ReTest.load("Hijack/test/load_path.jl",
                                                revise=true)
    end
end

# * Hijack ...................................................................

@chapter Hijack false begin
    Pkg.activate("./Hijack")
    Pkg.develop(PackageSpec(path="..")) # ReTest
    Pkg.test("Hijack")

    if VERSION < v"1.5"
        using Revise
        @test ReTest.get_revise(nothing) === nothing

    else

        using Hijack
        @test !haskey(ReTest.loaded_testmodules, Hijack)
        @test first.(process_args((Hijack,), load=true).modules) ==
            [
                HijackLoadTrueTests,
                HijackLoadTrueTests2
            ]
        Hijack_testmodules = ReTest.loaded_testmodules[Hijack]
        @test Hijack_testmodules == [HijackLoadTrueTests, HijackLoadTrueTests2]
        @test first.(process_args((Hijack,), load=true).modules) ==
            [
                HijackLoadTrueTests,
                HijackLoadTrueTests2
            ] == Hijack_testmodules # to be sure no module was overwritten

        ReTest.hijack(Hijack, testset=false) # testset: just check that this method
                                             # also accepts this kw (TODO: test it!)
        retest(HijackTests)
        @test Hijack.RUN == [1, 5, 4]
        empty!(Hijack.RUN)

        @test_throws ErrorException ReTest.hijack(Hijack, :HijackTests2, revise=true)
        # Revise not loaded || VERSION < v"1.5"
        @test_throws ErrorException ReTest.load(Hijack, "load_revise.jl",
                                                revise=true, parentmodule=Load)


        using Revise ###############################
        @test nameof(ReTest.get_revise(nothing)) == :Revise

        Test.@testset "load(revise=true)" begin
            # big hack, this should belong to the FakePackage chapter, but we can't load
            # Revise then, because we @test_throws above for Revise not loaded
            @test ReTest.load(Hijack, "../../FakePackage/test/FakePackageTests2.jl",
                              parentmodule=Load, revise=true) ==
                                  Load.AlternateFakePackageTests
            @test first.(process_args((Load.AlternateFakePackageTests,)).modules) ==
                [Load.AlternateFakePackageTests]

            # here, Load.HijackTests gets defined
            @test ReTest.load(Hijack, "load_revise.jl", parentmodule=Load) == # revise=true
                Load.HijackTests
            @test Load.load_revise_function() == 1
            @test Load.HijackTests.load_revise_function() == 1
            @test first.(process_args((Load.HijackTests,)).modules) == [Load.HijackTests]
            Load.HijackTests.check(Load.HijackTests, [1])

            HL = ReTest.load(Hijack, "HijackTestsLoad123.jl", parentmodule=Load)
            @test HL == [Load.HijackTestsLoad1, Load.HijackTestsLoad2]
            @test HL[1].f() == 1
            @test HL[2].f() == 1
            @test Load.HijackTestsLoad3.f() == 1

            lpf = ReTest.load("Hijack/test/load_path.jl", parentmodule=Load2)
            @test Load2.load_path_function() == 1
            @test lpf == Load2.load_path_function
        end

        ReTest.hijack(Hijack, :HijackTests2) # revise=true by default
        retest(HijackTests2)
        @test Hijack.RUN == [1, 5, 4]
        empty!(Hijack.RUN)

        # Submodules
        ReTest.hijack("Hijack/test/submodules_tests.jl", :SubMod1, revise=true)
        retest(SubMod1)
        @test SubMod1.RUN == [1]; empty!(SubMod1.RUN)
        @test SubMod1.SubModule.RUN == [1]; empty!(SubMod1.SubModule.RUN)
        @test SubMod1.SubModule.Sub.RUN == [1]; empty!(SubMod1.SubModule.Sub.RUN)

        ## UPDATING ######

        orig(file) = let s = splitext(file)
            join([s[1], ".orig", s[2]])
        end
        function update_file!(f, file)
            cp(file, orig(file), force=true)
            write(file, f(chomp(read(file, String))))
        end
        restore_file!(file) = mv(orig(file), file, force=true)

        # EDIT FILE 1
        sub_file = "./Hijack/test/subdir/sub.jl"
        update_file!(sub_file) do _
            """
@test true

@testset "sub" begin
    @test true
    @test true
    push!(Hijack.RUN, 2)
end
"""
        end

        # EDIT FILE 2
        load_revise = "./Hijack/test/load_revise.jl"
        update_file!(load_revise) do content
            content = replace(content,
                              "load_revise_function() = 1" => "load_revise_function() = 2")
            lines = split(content, r"\n|\r\n") # cf. https://github.com/JuliaLang/julia/pull/20390
            @assert endswith(lines[14], "@test true")
            @assert endswith(lines[15], "trace(1)")
            insert!(lines, 16, "@test true")
            insert!(lines, 17, "trace(2)")
            join(lines, '\n')
        end

        # EDIT FILES 3 & 4 & 5
        mod_revise = "Hijack/test/submodules_tests.jl"
        submod_revise = "Hijack/test/submodule.jl"
        subsubmod_revise = "Hijack/test/subsubmodule.jl"
        for sub in (mod_revise, submod_revise, subsubmod_revise)
            update_file!(sub) do content
                replace(content, "push!(RUN, 1)" => "push!(RUN, 2)")
            end
        end

        # EDIT FILE 6
        load_hijack123 = "Hijack/test/HijackTestsLoad123.jl"
        update_file!(load_hijack123) do content
            replace(content, "f() = 1" => "f() = 2")
        end

        # EDIT FILE 7
        load_path_file = "Hijack/test/load_path.jl"
        update_file!(load_path_file) do content
            replace(content, "load_path_function() = 1" => "load_path_function() = 2")
        end

        Revise.revise()
        try
            Test.@testset "revise works" begin
                retest(HijackTests2)
                @test Hijack.RUN == [2, 5, 4]
                Load.HijackTests.check(Load.HijackTests, [1, 2])
                @test Load.load_revise_function() == 2
                @test Load.HijackTests.load_revise_function() == 2
                retest(SubMod1)
                @test SubMod1.RUN == [2]; empty!(SubMod1.RUN)
                @test SubMod1.SubModule.RUN == [2]; empty!(SubMod1.SubModule.RUN)
                @test SubMod1.SubModule.Sub.RUN == [2]; empty!(SubMod1.SubModule.Sub.RUN)

                @test Load.HijackTestsLoad1.f() == 2
                @test Load.HijackTestsLoad2.f() == 2
                @test Load.HijackTestsLoad3.f() == 2
                @test Load2.load_path_function() == 2
            end
        finally
            restore_file!(sub_file)
            restore_file!(load_revise)
            restore_file!(mod_revise)
            restore_file!(submod_revise)
            restore_file!(subsubmod_revise)
            restore_file!(load_hijack123)
            restore_file!(load_path_file)
        end

        # test lazy=true
        empty!(Hijack.RUN)
        ReTest.hijack("./Hijack/test/lazy.jl", :HijackLazy, lazy=true)
        retest(HijackLazy)
        @test Hijack.RUN == [1, 3]

        # test lazy=:brutal
        empty!(Hijack.RUN)
        ReTest.hijack("./Hijack/test/lazy.jl", :HijackBrutal, lazy=:brutal)
        retest(HijackBrutal)
        @test Hijack.RUN == [3]

        # test lazy=:wrong
        empty!(Hijack.RUN)
        @test_throws ArgumentError ReTest.hijack("./Hijack/test/lazy.jl", :HijackWrong, lazy=:wrong)

        # test testset=true
        empty!(Hijack.RUN)
        ReTest.hijack("./Hijack/test/testset.jl", :HijackTestset, testset=true)
        retest(HijackTestset)
        @test Hijack.RUN == [1, 2, 3]
    end
end
