using FakePackage, ReTest

retest(FakePackage)
@test FakePackage.RUN == [1, 2]

FakePackage.runtests(r"begin-end")
@test FakePackage.RUN == [1, 2, 1]
FakePackage.runtests(r" for")
@test FakePackage.RUN == [1, 2, 1, 2]

retest(dry=true) # only to update InlineTest.TESTED_MODULES
@test FakePackage.Submodule in ReTest.TESTED_MODULES
