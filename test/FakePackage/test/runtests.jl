using FakePackage, ReTest
using ReTest: process_args, and
import ReTest: Test
include("../../setup.jl")

retest(FakePackage, recursive=false)
@test FakePackage.RUN == [1, 2]

FakePackage.runtests(r"begin-end")
@test FakePackage.RUN == [1, 2, 1]
FakePackage.runtests(r" for")
@test FakePackage.RUN == [1, 2, 1, 2]

retest(dry=true) # only to update InlineTest.TESTED_MODULES
@test FakePackage.Sub1 in ReTest.TESTED_MODULES
@test FakePackage.Sub2.SubSub in ReTest.TESTED_MODULES

retest(FakePackage, recursive=true)
@test FakePackage.RUN == [1, 2, 1, 2, 1, 2, 3, 4]
retest(FakePackage) # equivalent to recursive=true
@test FakePackage.RUN == [1, 2, 1, 2, 1, 2, 3, 4, 1, 2, 3, 4]

Test.@testset "test macros" begin
    empty!(FakePackage.FakeMacro.RUN)
    retest(FakePackage, "test macro", verbose=9)
    @test FakePackage.FakeMacro.RUN == [4, 2, 3]
end

# in the following, in FakePackage (not FakePackage.FakeMacro), the line `x = 3` is
# shown because statically, we don't know that 4!=2, which is the condition for not
# running the same macro on 3
check(FakePackage, "test macro", verbose=9, id=false, dry=true, [], output="""
FakePackage
  test macro
    inner
      macro begin x = 4
      macro for 1
        plus
      macro begin x = 3
      macro for 1
        plus

FakePackage.FakeMacro
  test macro
    macro begin x = 2
    macro for 1
      plus
    macro begin x = 3
    macro for 1
      plus
""")

module Included ############################################
using ReTest
RUN = []

@testset "include parent" begin
    include("included.jl")
    include(joinpath(@__DIR__, "included.jl"))
    if VERSION >= v"1.5"
        include(identity, "included.jl")
        include(identity, joinpath(@__DIR__, "included.jl"))
    end
end
end # Included ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

retest(Included)
@test Included.RUN == (VERSION >= v"1.5" ? [0, 0, 0, 0] : [0, 0])

ReTest.Test.@testset "retest: load" begin
    @test process_args(()).modules == [
        FakePackage => and(),
        FakePackage.FakeMacro => and(),
        FakePackage.Sub1 => and(),
        FakePackage.Sub2.SubSub => and(),
        Main.Included => and(),
    ]
    check([], dry=true, verbose=0, output = """
FakePackage
FakePackage.FakeMacro
FakePackage.Sub1
FakePackage.Sub2.SubSub
Main.Included
""")

    check([], dry=true, verbose=0, load=true, output = """
FakePackage
FakePackage.FakeMacro
FakePackage.Sub1
FakePackage.Sub2.SubSub
Main.Included
Main.FakePackageTests
Main.FakePackageTests.Sub
Main.FakePackageTests.Sub.SubSub
""")

    check(FakePackage, [], dry=true, verbose=0, load=true, recursive=false, output = """
FakePackage
Main.FakePackageTests
""")

    @test process_args((FakePackage, 2)).modules ==
        [
            FakePackage => and(2),
            FakePackage.FakeMacro => and(2),
            FakePackage.Sub1 => and(2),
            FakePackage.Sub2.SubSub => and(2),
        ]
    @test process_args((FakePackage, 2), load=true, recursive=false).modules ==
        [
            FakePackage => and(2),
            FakePackageTests => and(2),
        ]
    # cumulative patterns
    @test process_args((FakePackage, 2, FakePackageTests => 3),
                       load=true, recursive=false).modules ==
        [
            FakePackage => and(2),
            FakePackageTests => and(3, 2),
        ]
    # specified pattern for test module
    @test process_args((FakePackage => 2, FakePackageTests => 3),
                       load=true, recursive=false).modules ==
        [
            FakePackage => and(2),
            FakePackageTests => and(3),
        ]
    @test process_args((FakePackage => 2, FakePackageTests => 3),
                       load=true, recursive=true).modules ==
        [
            FakePackage => and(2),
            FakePackageTests => and(3),
            FakePackage.FakeMacro => and(2),
            FakePackage.Sub1 => and(2),
            FakePackage.Sub2.SubSub => and(2),
            FakePackageTests.Sub => and(3),
            FakePackageTests.Sub.SubSub => and(3),
        ]
end
