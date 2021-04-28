using FakePackage, ReTest

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

module Included
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
end

retest(Included)
@test Included.RUN == (VERSION >= v"1.5" ? [0, 0, 0, 0] : [0, 0])

include("../../setup.jl")

ReTest.Test.@testset "retest: load" begin
    check([], dry=true, verbose=0, output = """
FakePackage
FakePackage.Sub1
FakePackage.Sub2.SubSub
Main.Included
""")

    check([], dry=true, verbose=0, load=true, output = """
FakePackage
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
end
