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
