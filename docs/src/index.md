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

`ReTest` is still at an early stage of development. If you like to use it
for your packages, it's recommended to [keep your tests compatible with
`Test`](@ref Keeping-the-ability-to-use-Test), so that they can be run through
both frameworks (e.g. using `ReTest` interactively and `Test` in C.I.).


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
`runtests` function within the enclosing module (and within all recursive
parent modules), say `M`, such that `M.runtests(...)` is equivalent to
calling `retest(M, ...)`.


## Quick start

Both `ReTest` and `InlineTest` are registered packages and can be installed
the usual way. Let's create a package `MyPackage`, which is initially a
single file located in a directory known to `LOAD_PATH`. We want to test
its `greet` function, by writing tests directly in the same file;
this is a use-case for `InlineTest`, which loads faster than `ReTest`:
```julia
# MyPackage.jl file

module MyPackage
using InlineTest

greet() = "Hello World!"

@testset "greet" begin
    @test greet() == "Hello World!"
end

end # module
```

Now, in a Julia session, we load `MyPackage` and `ReTest` (needed to actually
run the tests):
```julia
julia> using MyPackage, ReTest

julia> MyPackage.runtests()
             Pass
greet    |      1
```

Suppose now that we organize `MyPackage` as a standard package, with
a proper "runtests.jl" file. We can still keep testsets within "MyPackage.jl",
while adding more thorough tests in the "test" folder, which can contain
two files, "runtests.jl" and "MyPackageTests.jl":
```julia
# MyPackage/test/runtests.jl file

using ReTest, MyPackage
include("MyPackageTests.jl")

# when including this file (e.g. with `Pkg.test`), all the tests
# in both modules will be run:

retest(MyPackage, MyPackageTests)
```

```julia
# MyPackage/test/MyPackageTests.jl file

module MyPackageTests
using MyPackage, ReTest

@testset "more greet" begin
    @testset "concatenation" begin
        @test MyPackage.greet()^2 == "Hello World!Hello World!"
    end
end

@testset "stuff" begin
    @test true
end

end # module
```

We can now load tests either via `using MyPackageTests`, if `LOAD_PATH` is configured
appropriately, or via `include`, and run whichever tests we want:
```julia
julia> include("test/MyPackageTests.jl");

julia> using ReTest # to use the `retest` function

julia> retest(dry=true, verbose=2) # just list tests, showing nested ones
MyPackage
1| greet

Main.MyPackageTests
1| more greet
2|   concatenation
3| stuff

julia> retest("greet", verbose=2) # run only tests related to `greet()`
                         Pass
MyPackage:
  greet              |      1

Main.MyPackageTests:
  more greet         |      1
    concatenation    |      1

Overall              |      2

julia> MyPackageTests.runtests(3) # run only testset with ID 3 in MyPackageTests
                          Pass
3| stuff              |      1
```

Here it is for basic usage!


## API

```@meta
CurrentModule = ReTest
```

* [Defining tests](@ref)
* [Running tests](@ref)
* [Loading tests](@ref)
* [Filtering tests](@ref)

### Defining tests

* [`@testset`](@ref)
* [`@testset_macro`](@ref)

```@docs
@testset
@testset_macro
```

### Running tests

* [`retest`](@ref)
* [`watch`](@ref)

```@docs
retest
watch
```

### Loading tests

* [`load`](@ref)
* [`hijack`](@ref)
* [`hijack_base`](@ref)

Cf. [Working with test files which use Test](@ref) for  `hijack` and `hijack_base`.

```@docs
load
```

### Filtering tests

* [`not`](@ref)
* [`pass`](@ref) and `fail`
* [`reachable`](@ref)
* [`interpolated`](@ref)
* [`depth`](@ref)

```@docs
not
pass
reachable
interpolated
depth
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

* Testsets can not be "custom testsets" (cf. `Test` documentation).

* Nested testsets can't be "qualified" (i.e. written as `ReTest.@testset`).

* Regex filtering logic might improve in future versions, which means that
  with the same regex, less tests might be run (or more!). See
  [`retest`](@ref)'s docstring to know which testsets are guaranteed to run.

* Descriptions of testsets must be unique within a module, otherwise they are
  overwritten and a warning is issued, unless `Revise` is loaded; the reason
  is the current implemented heuristic to allow `Revise` do its magic.

* There is not yet a good solution to factor out testsets into functions called
  within other testsets; a work-around is to use [`@testset_macro`](@ref),
  or to use only the `@test` and `@test_*` macros within these functions.

* Interrupting running testsets with Control-C sometimes doesn't work well,
  because of the use of multiple tasks in `retest`. There can also be
  usability annoyances when some tests are failing. This will hopefully
  be fixed _soon_.


#### Including files from within testsets

TLDR: don't use `include(file)` within testsets when `file` defines
other testsets.

There is limited support for `include(path)` expressions within testsets: all
what `ReTest` does is to adjust the `path` according to the location of the
containing file `parentfile`. This is necessary, because `include` is not run
immediately when that file is evaluated; when the given testset is triggered
(via a `retest` call), `include` doesn't have the same "context" as
`parentfile`, which would lead to `path` being interpreted as non-existing
(unless `path` is an absolute path). So when parsing testsets, `ReTest`
prepends the directory name of `parentfile` to `path`.

The important point is that `include` is executed at `retest`-time; if the
included file defines other `@testset` expressions, this will define new
testsets in the enclosing module, but these won't be run immediately; upon a
new `retest()` invocation, these new testsets will be run, but the old one too
(the one containing `include`), which will redefine included testsets. This is
brittle, and it's recommended to not include, within testsets, files defining
other testsets.


## Switching from `Test` to `ReTest`

When used in a package `MyPackage`, the recommended way to organize
test code is as follows:
1. replace `using Test` by `using ReTest` in the "runtests.jl" file (and in all
   other test files having `using Test`)
2. wrap the whole content of "runtests.jl" within a module named
   `MyPackageTests`
3. rename "runtests.jl" to "MyPackageTests.jl"
4. create a "runtests.jl" file with the following content:
   `include("MyPackageTests.jl"); MyPackageTests.runtests()`

This means that running "runtests.jl" will have the same net effect as before.
The "MyPackageTests.jl" file can now be `include`d in your REPL session
(`include("MyPackageTests.jl")`), and you can run all or some of its tests (e.g.
`MyPackageTests.runtests("addition")`).
This test file can also be included via the [`ReTest.load`](@ref) function
or via the `load` keyword of `retest`.


Wrapping the tests in `MyPackageTests` allows to not pollute `Main` and keeps
the tests of different packages separated. Also, you can
modify "MyPackageTests.jl" and re-include it to have the corresponding tests updated
(the `MyPackageTests` module is replaced in `Main`);
otherwise, without a `MyPackageTests` module, including the file a second
time currently triggers a warning for each overwritten toplevel testset.


#### Keeping the ability to use `Test`

One might want to have the possibility to use either `Test` or `ReTest` depending on the
context. Reasons to still use `Test` include:
* when running `retest` for the first time in a Julia session, more code has
  to be compiled than when running tests with `Test`, so in the case of
  running the whole test suite, few seconds can be spared (although using
  `ReTest` in parallel mode would generally compensate for this);
* `ReTest` is not yet a fully mature and battle tested package, so you might
  want to not rely exclusively on it, e.g. for C.I.

An alternate way to organize the test files is as follows, assuming `using Test` is only
present in "runtests.jl":
1. remove `using Test` from "runtests.jl"
2. rename "runtests.jl" to "tests.jl"
3. create a "MyPackageTests.jl" file with the following content:
   ```julia
   module MyPackageTests
   using ReTest
   include("tests.jl")
   end
   ```
4. create a "runtests.jl" file with the following content:
   ```julia
   using Test
   include("tests.jl")
   ```

That way, `include("test/runtests.jl")` or `Pkg.test()` will run tests using `Test`,
while `include("test/MyPackageTests.jl"); MyPackageTests.runtests()` will use `ReTest`.


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
@everywhere include("test/MyPackageTests.jl")
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
"test/MyPackageTests.jl" file, you can request `Revise` to "load" the updated
testsets by putting `__revise_mode__ = :eval` in the enclosing module.

When files are included recursively, plain `includet` won't work
(it is currently documented to be "deliberately non-recursive").
There are three work-arounds, of which the first is recommended:
1. load `MyPackageTests` as a module, i.e. via `using MyPackageTests` instead
   of `include("test/MyPackageTests.jl")` (this might involve updating your
   `LOAD_PATH` to include "test/" and making sure the required packages are
   found)
3. load `MyPackageTests` via [`ReTest.load(MyPackage, revise=true)`](@ref), but this works
   only in "simple enough" situations
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
  `retest`. To have `Revise` track changes to test files, use
  `ReTest.hijack(Package, revise=true)`.
- if you have a test file `"testfile.jl"`, try `ReTest.hijack("testfile.jl")`
  (this will define a fresh module like above).
- `Base` and standard library modules can also be passed to `ReTest.hijack`
  (corresponding tests are loaded via the lower level [`ReTest.hijack_base`](@ref)
  function).

```@docs
ReTest.hijack
ReTest.hijack_base
```
