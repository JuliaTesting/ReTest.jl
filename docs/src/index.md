# ReTest.jl

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
`Base`/stdlib tests), cf. [Working with test files which use `Test`](@ref).


## Usage

The exported [`ReTest.@testset`](@ref) macro can be used as a direct
replacement for `Test.@testset` (with limitations, [see below](@ref Caveats)),
and `retest()` has to be called for the tests to be executed. See
[`retest`](@ref)'s docstrings for more details. Moreover, `ReTest` re-exports
(almost) all exported symbols from `Test`, so there should not be any need to
import `Test` together with `ReTest`.

When using `@testset` "inline", i.e. within the source-code of a package, one
can use the `InlineTest` package instead of `ReTest`, which only defines the
strict minimum and also exports `@testset`, and therefore loads faster (even
if `ReTest` itself loads fast, it can be desirable to have an even lighter
dependency). But `ReTest` still has to be loaded (as a "test" dependency) in
order to call `retest`.

Finally, for convenience, `ReTest.@testset` also implicitly defines a
`runtests` function within the enclosing module, say `M`, such that
`M.runtests(...)` is equivalent to calling `retest(M, ...)`.


## `retest` and `@testset`

```@meta
CurrentModule = ReTest
```

```@docs
retest
@testset
```


## Caveats

`ReTest.@testset` comes with a couple of caveats/limitations, some of which
should be fixable:

* Toplevel testsets (which are not nested within other testsets), when run,
  are `eval`ed at the toplevel of their parent module, which means that they
  can't depend on local variables for example.

* "testsets-for" (`@testset "description" for ...`), when run, imply `eval`ing
  their loop variables at the toplevel of their parent module; this implies
  that iteration expressions shouldn't depend on local variables (otherwise,
  the testset subject usually can't be known statically and the testset can't
  be filtered out with a `Regex`).

* "testsets-for" currently accept only "non-cartesian" looping (e.g. `for i=I,
  j=J` is not supported, PRs welcome!)

* Testsets can not be "custom testsets" (cf. `Test` documentation).

* Nested testsets can't be "qualified" (i.e. written as `ReTest.@testset`).

* Regex filtering logic might improve in future versions, which means that
  with the same regex, less tests might be run (or more!). See
  [`retest`](@ref)'s docstring to know which testsets are guaranteed to run.

* Descriptions of testsets must be unique within a module, otherwise they are
  overwritten and a warning is issued, unless `Revise` is loaded; the reason
  is the current implemented heuristic to allow `Revise` do its magic.


## Switching from `Test` to `ReTest`

When used in a package `MyPackage`, the test code can be organized as follows:
1. replace `using Test` by `using ReTest` in the "runtests.jl" file
2. wrap the whole content of "runtests.jl" within a module named
   `MyPackageTests`
3. rename "runtests.jl" to "tests.jl"
4. create "runtests.jl" with the following content:
   `include("tests.jl"); MyPackageTests.runtests()`

This means that running "runtests.jl" will have the same net effect as before.
The "tests.jl" file can now be `include`d in your REPL session
(`include("tests.jl")`), and you can run all or some of its tests (e.g.
`MyPackageTests.runtests("addition")`).

Wrapping the tests in `MyPackageTests` allows to not pollute `Main` and keeps
the tests of different packages separated. Also, you can
modify "tests.jl" and re-include it to have the corresponding tests updated
(the `MyPackageTests` module is replaced in `Main`);
otherwise, without a `MyPackageTests` module, including the file a second
time currently triggers a warning for each overwritten toplevel testset.


## Filtering

Most of the time, filtering with a simple string is likely to be enough. For example, in
```julia
@testset "a" begin
    @test true
    @testset "b" begin
    end
    @testset "c" begin
    end
end
```

running `retest(M, "a")` will run everything, `retest(M, "b")` will run
`@test true` and `@testset "b"` but not `@testset "c"`.
Note that if you want to run `@testset "b"`, there is no way to not run
`@test true` in `@testset "a"`; so if it was an expensive test to run,
instead of `@test true`, it could be useful to wrap it in its own testset, so that
it can be filtered out.


## Running tests in parallel with `Distributed`

Currently, the tests are automatically run in parallel whenever there are
multiple workers, which have to be set manually. Running the tests looks like:
```julia
using Distributed
addprocs(2)
@everywhere include("test/tests.jl")
MyPackageTests.runtests()
```

If the test code doesn't use `ReTest`
(cf. [Working with test files which use `Test`](@ref)), this can be done as
follows:
```julia
using Distributed
addprocs(2)
using ReTest
@everywhere begin
    using ReTest, MyPackage
    ReTest.hijack(MyPackage)
end
MyPackageTests.runtests()
```

!!! note

    As was already mentioned, testset-for iterators are evaluated at load time in
    the enclosing module, but this currently happens only in the main process.
    This can lead to unexpected errors when the package was written without a
    `Distributed` use-case in mind.

    For example, say the package defines a constant singleton object `X` which is
    normally equal to itself (because `X === X`). But if `X` is assigned to a
    testset-for loop variable `x`, it will be the one from the main process, so
    within the testset-for, a test like `x == X` might fail because `X` refers to
    the singleton object defined in another process; a solution in this case could
    be to define explicitly `==` for objects of the type of `X`.

It should be relatively easy to support threaded execution of testsets (it was
actually implemented at one point). But it often happens that compiling
package code and testset code (which currently is not threaded) takes quite
more time than actually running the code, in which case using `Distributed`
has more tangible benefits.


## Working with `Revise`

When `Revise` is loaded and a testset is updated, `ReTest` will observe that a
new testset is added with the same description as a previously existing one,
which is then overwritten. This works only if the description is not modified,
otherwise both the old and new versions of the testset will co-exist.

For testsets in a "script" loaded with `includet`, e.g. those in a
"test/tests.jl" file, you can request `Revise` to "load" the updated testsets by
putting `__revise_mode__ = :eval` in the enclosing module.

When files are included recursively, plain `includet` won't work
(it is currently documented to be "deliberately non-recursive").
There are two work-arounds:
1. rename your "test/tests.jl" file to "test/MyPackageTests.jl" and load it as a module
   (this might involve updating your `LOAD_PATH` to include "test/" and making sure
   the required packages are found)
2. use the [following `recursive_includet`](https://github.com/timholy/Revise.jl/issues/518#issuecomment-667097500)
   function instead of `includet`:
```julia
function recursive_includet(filename)
    already_included = copy(Revise.included_files)
    includet(filename)
    newly_included = setdiff(Revise.included_files, already_included)
    for (mod, file) in newly_included
        Revise.track(mod, file)
    end
end
```


## Working with test files which use `Test`

It's sometimes possible to use `ReTest` features on a test code base which
uses `Test`, without modifications:

- if you have a package `Package`, you can try `ReTest.hijack(Package)`, which
  will define a `PackageTests` module when successful, on which you can call
  `retest`.
- if you have a test file `"testfile.jl"`, try `ReTest.hijack("testfile.jl")`
  (this will define a fresh module like above).
- `Base` and standard libraries tests can be loaded via the [`ReTest.hijack_base`](@ref)
  function.

```@docs
ReTest.hijack
ReTest.hijack_base
```
