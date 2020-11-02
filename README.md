# ReTest

This package allows:

1. defining tests in source files, whose execution is deferred and triggered on demand.

   This is useful when one likes to have definitions of methods and
   corresponding tests close to each other. This is also useful for code which
   is not (yet) organized as a package, and where one doesn't want to maintain a
   separate set of files for tests.

2. filtering run testsets with a `Regex`, which is matched against the
   descriptions of testsets.

   This is useful for running only part of the test suite of a package. For
   example, if you made a change related to addition, and included "addition" in
   the description of the corresponding tetstsets, you can easily run only these
   tests.

   Note that a [pull request](https://github.com/JuliaLang/julia/pull/33672)
   exists in the Julia repository to implement regex-filtering for
   `Test.@testset`.

The exported `ReTest.@testset` macro can be used as a direct replacement for
`Test.@testset`, and `runtests()` has to be called for the tests to be executed.
See the docstrings (reproduced below) for more details. Moreover, `ReTest`
re-exports (almost) all exported symbols from `Test`, so there should not be any
need to import `Test` together with `ReTest`.

### `runtests` docstring

```
    runtests([m::Module], pattern = r""; [wrap::Bool])
```
Run all the tests declared in `@testset` blocks, within `m` if specified,
or within all currently loaded modules otherwise.
The `wrap` keyword specifies whether the collection of `@testset` expressions
should be grouped according to the parent modules within a top-level `@testset`.
The default is `wrap=false` when `m` is specified, `true` otherwise.

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
A testset is guaranteed to run only when its parent's subject "partially matches"
`pattern::Regex` (in PCRE2 parlance, i.e. `subject` might be the prefix of a string
matched by `pattern`) and its subject matches `pattern`.

If the passed `pattern` is a string, then it is wrapped in a `Regex` prefixed with
`".*"`, and must match literally the subjects.
This means for example that `"a|b"` will match a subject like `"a|b"` but not like `"a"`.
The `".*"` prefix is intended to allow matching subjects of nested testsets,
e.g. in the example above, `r".*b"` partially matches the subject `"/a"` and
matches the subject `"/a/b"` (so the corresponding nested testset is run),
whereas `r"b"` does not partially match `"/a"`, even if it matches `"/a/b"`,
so the testset is not run.

Note: this function executes each (top-level) `@testset` block using `eval` *within* the
module in which it was written (e.g. `m`, when specified).

### Caveats

`ReTest.@testset` comes with a couple of caveats/limitations:

* Toplevel testsets (which are not nested within other testsets), when run, are
  `eval`ed at the toplevel of their parent module, which means that they can't
  depend on local variables for example. This is probably the only fundamental
  limitation compared to `Test.@testset`, and might not be fixable.

* Toplevel "testsets-for" (`@testset "description" for ...`), when run, imply
  `eval`ing their loop variables at the toplevel of their parent module;
  moreover, "testsets-for" accept only "non-cartesian" looping (e.g. `for i=I,
  j=J` is not supported). Both problems should be fixable.

  Related to the previous point: in future versions, nested testsets-for might
  have their "iterator" (giving the values their loop variables) `eval`ed at the
  module's toplevel (for efficiency).

* Testsets can not be "custom testsets" (cf. `Test` documentation; this should
  be easy to support).

* Nested testsets can't be "qualified" (i.e. written as `ReTest.@testset`).

* Regex filtering logic might improve in future versions, which means that with
  the same regex, less tests might be run (or more!). See `runtests`'s docstring
  to know what testsets are guaranteed to run.

### Switching from `Test` to `ReTest`

When used in a package `MyPackage`, the test code can be organized as follows:
1. replace `using Test` by `using ReTest` in the "runtests.jl" file
2. wrap the whole content of "runtests.jl" within a module named `MyPackageTests`
3. rename "runtests.jl" to "tests.jl"
4. create "runtests.jl" with the following content:
   `include("tests.jl"); MyPackageTests.runtests(MyPackageTests)`

This means that running "runtests.jl" will have the same net effect as before.
The "tests.jl" file can now be `include`d in your REPL session (`include("tests.jl")`),
and you can run all or some of its tests
(e.g. `using ReTest; runtests(MyPackageTests, "addition")`).
Wrapping the tests in `MyPackageTests` allows to not pollute `Main`, it keeps the tests
of different packages separated, but more importantly, you can modify "tests.jl" and
re-include it to have the corresponding tests updated (otherwise, without
a `MyPackageTests` module, including the file a second time would add the new tests
without removing the old ones).

### Toplevel testsets

In `ReTest`, toplevel testsets are special, as already mentioned. The fact
that they are `eval`ed at the module's toplevel also means that we can actually
filter them out (if a filtering pattern is given to `runtests`) "statically",
i.e. by introspection, we can figure out whether they should be run before
having to `eval` the corresponding code. This is a big win, in particular for
`testsets-for`, which are expensive to compile. This allows `ReTest` to
compete with the good-old manual way of copy-pasting the wanted `@testset` into
the REPL (without this trick, all the testsets would have to be `eval`ed, even
when they don't run any code, and this can take some time for large test
codebases.

So the recommendation for efficient filtering of the tests is to favor toplevel
testsets, e.g.

```julia
@testset "a" begin
# ...
end
@testset "b" begin
# ...
end
# etc.
```
instead of
```julia
@testset "everything" begin
    @testset "a" begin
    # ...
    end
    @testset "b" begin
    # ...
    end
    # etc.
end
```

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

running `runtests(M, "a")` will run everything, `runtests(M, "b")` will run
`@test true` and `@testset "b"` but not `@testset "c"`. Using `Regex` is
slightly more subtle. For example, `runtests(M, r"a")` is equivalent to
`runtests(M, "a")` in this case, but `runtests(M, r"b")` will run nothing,
because `r"b"` doesn't "partially match" the toplevel `@testset "a"`; you would
have to use `r".*b"` instead for example, or `r"a/b"` (or even `r"/b"`).

Note also that partial matching often gives a false positive, e.g. running
`runtests(M, "d")` will currently instantiate `@testset "a"` because it might
contain a sub-testset `"d"`, so `@test true` above will be run, even if
eventually no testset matches `"d"`. So it's recommended to put expensive tests
withing "final" testsets (those which don't have nested testsets), such that
"full matching" is used instead of partial matching.
