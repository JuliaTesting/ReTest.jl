using FakePackage, ReTest

runtests(FakePackage)
@test FakePackage.RUN == [1, 2]

runtests(FakePackage, r"begin-end")
@test FakePackage.RUN == [1, 2, 1]
runtests(FakePackage, r" for")
@test FakePackage.RUN == [1, 2, 1, 2]
