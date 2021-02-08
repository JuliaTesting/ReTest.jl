# ReTest

[![Tests Status](https://github.com/JuliaTesting/ReTest.jl/workflows/CI/badge.svg)](https://github.com/JuliaTesting/ReTest.jl/actions?query=workflow%3ACI)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaTesting.github.io/ReTest.jl/stable)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://JuliaTesting.github.io/ReTest.jl/dev)

`ReTest` is a testing framework for Julia allowing:

1. Defining tests in source files, whose execution is deferred and triggered
   on demand.

   This is useful when one likes to have definitions of methods and
   corresponding tests close to each other. This is also useful for code which
   is not (yet) organized as a package, and where one doesn't want to maintain
   a separate set of files for tests.

2. Filtering run testsets with a `Regex`, which is matched against the
   descriptions of testsets.

   This is useful for running only part of the test suite of a package. For
   example, if you made a change related to addition, and included "addition"
   in the description of the corresponding testsets, you can easily run only
   these tests.

   Note that a [pull request](https://github.com/JuliaLang/julia/pull/33672)
   exists in the Julia repository to implement regex-filtering for
   `Test.@testset`.

A couple more features are also enabled, like shuffling the order in which
the testsets are run, or running testsets in parallel (via `Distributed`).

`ReTest` is mostly backward-compatible with `Test`, i.e. minimal change to
test files is necessary in order to switch to `ReTest`; it's often even
possible to use `ReTest` features without changing a line, e.g. on Julia's
`Base`/stdlib tests.
