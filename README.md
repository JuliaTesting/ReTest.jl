# ReTest

[![Build Status](https://travis-ci.com/JuliaTesting/ReTest.jl.svg?branch=master)](https://travis-ci.com/JuliaTesting/ReTest.jl)

This package allows:

1. Defining tests in source files, whose execution is deferred and triggered on demand.

   This is useful when one likes to have definitions of methods and
   corresponding tests close to each other. This is also useful for code which
   is not (yet) organized as a package, and where one doesn't want to maintain a
   separate set of files for tests.

2. Filtering run testsets with a `Regex`, which is matched against the
   descriptions of testsets.

   This is useful for running only part of the test suite of a package. For
   example, if you made a change related to addition, and included "addition" in
   the description of the corresponding testsets, you can easily run only these
   tests.

   Note that a [pull request](https://github.com/JuliaLang/julia/pull/33672)
   exists in the Julia repository to implement regex-filtering for
   `Test.@testset`.

The exported `ReTest.@testset` macro can be used as a direct replacement for
`Test.@testset` (with limitations, see below), and `retest()` has to be called
for the tests to be executed. See the docstrings (reproduced below) for more
details. Moreover, `ReTest` re-exports (almost) all exported symbols from
`Test`, so there should not be any need to import `Test` together with `ReTest`.

When using `@testset` "inline", i.e. within the source-code of a package, one
can use the `InlineTest` package instead of `ReTest`, which only defines the
strict minimum and also exports `@testset`, and therefore loads faster (even if
`ReTest` itself loads fast, it can be desirable to have an even lighter
dependency). But `ReTest` still has to be loaded (as a "test" dependency) in
order to call `retest`.

Finally, for convenience, `@testset` also implicitly defines a `runtests` function
within the enclosing module, say `M`, such `M.runtests(...)` is equivalent to
calling `retest(M, ...)`.

### `retest` docstring

```
    retest([m::Module], pattern = r""; dry::Bool=false, stats::Bool=false,
                                       shuffle::Bool=false)
```
Run all the tests declared in `@testset` blocks, within `m` if specified,
or within all currently loaded modules otherwise.
If `dry` is `true`, don't actually run the tests, just print the descriptions
of the testsets which would (presumably) run.
If `stats` is `true`, print some time/memory statistics for each testset.
If `shuffle` is `true`, shuffle the order in which top-level testsets are run.

It's possible to filter run testsets by specifying `pattern`: the "subject" of a
testset is the concatenation of the subject of its parent `@testset`, if any,
with `"/$description"` where `description` is the testset's description.
For example:
```julia
@testset "a" begin # subject == "/a"
    @testset "b" begin # subject is "/a/b"
    end
    @testset "c$i" for i=1:2 # subjects are "/a/c1" & "/a/c2"
    end
end
```
A testset is guaranteed to run only when its subject matches `pattern`.
Moreover if a testset is run, its enclosing testset, if any, also has to run
(although not necessarily exhaustively, i.e. other nested testsets
might be filtered out).

If the passed `pattern` is a string, then it is wrapped in a `Regex` with the
"case-insensitive" flag, and must match literally the subjects.
This means for example that `"a|b"` will match a subject like `"a|b"` or `"A|B"`,
but not like `"a"` (only in Julia versions >= 1.3; in older versions,
the regex is simply created as `Regex(pattern, "i")`).

Note: this function executes each (top-level) `@testset` block using `eval` *within* the
module in which it was written (e.g. `m`, when specified).

### Caveats

`ReTest.@testset` comes with a couple of caveats/limitations:

* Toplevel testsets (which are not nested within other testsets), when run, are
  `eval`ed at the toplevel of their parent module, which means that they can't
  depend on local variables for example. This is probably the only fundamental
  limitation compared to `Test.@testset`, and might not be fixable.

* "testsets-for" (`@testset "description" for ...`), when run, imply
  `eval`ing their loop variables at the toplevel of their parent module;
  moreover, "testsets-for" accept only "non-cartesian" looping (e.g. `for i=I,
  j=J` is not supported). Both problems should be fixable.

* Testsets can not be "custom testsets" (cf. `Test` documentation; this should
  be easy to support).

* Nested testsets can't be "qualified" (i.e. written as `ReTest.@testset`).

* Regex filtering logic might improve in future versions, which means that with
  the same regex, less tests might be run (or more!). See `retest`'s docstring
  to know what testsets are guaranteed to run.

### Switching from `Test` to `ReTest`

When used in a package `MyPackage`, the test code can be organized as follows:
1. replace `using Test` by `using ReTest` in the "runtests.jl" file
2. wrap the whole content of "runtests.jl" within a module named `MyPackageTests`
3. rename "runtests.jl" to "tests.jl"
4. create "runtests.jl" with the following content:
   `include("tests.jl"); MyPackageTests.runtests()`

This means that running "runtests.jl" will have the same net effect as before.
The "tests.jl" file can now be `include`d in your REPL session (`include("tests.jl")`),
and you can run all or some of its tests
(e.g. `MyPackageTests.runtests("addition")`).
Wrapping the tests in `MyPackageTests` allows to not pollute `Main`, it keeps the tests
of different packages separated, but more importantly, you can modify "tests.jl" and
re-include it to have the corresponding tests updated (otherwise, without
a `MyPackageTests` module, including the file a second time would add the new tests
without removing the old ones).

### Filtering

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
