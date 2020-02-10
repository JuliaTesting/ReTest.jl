# InlineTest

This package allows defining tests in source files, whose execution is deferred and triggered on demand.
This is useful when one likes to have definitions of methods and corresponding tests close to each other.
This is also useful for code which is not (yet) organized as a package, and where one doesn't want to maintain
a separate set of files for tests.

The `@addtest` macro can be used as a direct replacement for `@testset`, and `runtests()` has to
be called for the tests to be executed. See the docstrings for more details.
When used in a package `MyPackage`, the following can be added to its `test/runtests.jl` file
in order to have `@addtest` tests run as part of the usual package testing process: `runtests(MyPackage)`
