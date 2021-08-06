"""
    ReTest.load(testpath::AbstractString;
                parentmodule::Module=Main, [revise::Bool])

Include file `testpath` into `parentmodule`. If `revise` is `true`, `Revise`,
which must be loaded beforehand in your Julia session, is used to track all
recursively included files (in particular testsets). The `revise` keyword
defaults to `true` when `Revise` is loaded and `VERSION >= v"1.5"`, and to
`false` otherwise.

The point of using this function is when `revise` is `true` and in particular
when files are included recursively.
If `revise` is false, this is equivalent to `parentmodule.include(testpath)`,
and if there are no recursively included files, this should be equivalent
to `Revise.includet(testpath)`, provided `parentmodule == Main` and
all `@testset`s defined in `testpath` are in a module defining
 `__revise_mode__ = :eval`.

!!! compat "Julia 1.5"
    This function requires at least Julia 1.5 when `revise` is `true`.
"""
function load(testpath::AbstractString;
              parentmodule::Module=Main, revise::Maybe{Bool}=nothing)

    revise === true && VERSION < v"1.5" &&
        error("the `revise` keyword requires at least Julia 1.5")
    Revise = get_revise(revise)

    if Revise === nothing
        Base.include(parentmodule, testpath)
    else
        files = Dict{String,Module}(testpath => parentmodule)
        substitute!(x) = substitute_retest!(x, false, false, files; ishijack=false)
        res = Base.include(substitute!, parentmodule, testpath)
        revise_track(Revise, files)
        res
    end
end

"""
    ReTest.load(Mod::Module, testfile::AbstractString="ModTests.jl";
                parentmodule::Module=Main, [revise::Bool])

Given a package `Mod`, include into `parentmodule`
the corresponding tests from file `testfile`, which is assumed to be
located in the "test" directory of the package.
It is expected that a unique new test module is created within `parentmodule`
after the inclusion, which is then returned.
Otherwise, the list of all newly created test modules is returned, triggering
a warning if it's empty.

If `revise` is `true`, `Revise`, which must be loaded beforehand in your Julia
session, is used to track the test files (in particular testsets). Note that
this might be brittle, and it's recommended instead to load your test module
via `using ModTests`. The `revise` keyword defaults to `true` when `Revise` is
loaded and `VERSION >= v"1.5"`, and to `false` otherwise.

!!! compat "Julia 1.5"
    This function requires at least Julia 1.5 when `revise` is `true`.
"""
function load(packagemod::Module, testfile::Maybe{AbstractString}=nothing;
              parentmodule::Module=Main, revise::Maybe{Bool}=nothing,
              maybe::Bool=false)

    newly_loaded = Module[]
    packagepath = pathof(packagemod)
    if packagepath === nothing
        maybe ? (return newly_loaded) : error("$packagemod is not a package")
    end
    default_testfile = string(packagemod, "Tests.jl")
    testfile = something(testfile, default_testfile)
    testpath = joinpath(dirname(dirname(packagepath)), "test", testfile)
    if !isfile(testpath)
        maybe ? (return newly_loaded) : error("file $testpath does not exist")
    end

    old_loaded = copy(update_TESTED_MODULES!())
    load(testpath; parentmodule=parentmodule, revise=revise)
    newly_loadedpars = parentmodules.(setdiff(update_TESTED_MODULES!(), old_loaded))
    for lpars in newly_loadedpars
        idx = findlast(==(parentmodule), lpars)
        if idx === nothing
            @warn "test module $(first(lpars)) was defined but is not a (recursive) submodule of $parentmodule"
        elseif idx == 1
            @warn "testsets were added directly into module $parentmodule"
        else
            root = lpars[idx-1]
            root ∈ newly_loaded || push!(newly_loaded, root)
        end
    end
    if testfile == default_testfile
        loaded_testmodules[packagemod] = newly_loaded
    end
    if !maybe && length(newly_loaded) == 1
        newly_loaded[1]
    else
        isempty(newly_loaded) &&
            @warn "test file $testpath loaded but it did not define any test module within $parentmodule"
        newly_loaded
    end
end

const loaded_testmodules = Dict{Module,Vector{Module}}()

"""
    ReTest.hijack(source, [modname];
                  parentmodule::Module=Main, lazy=false, [include::Symbol],
                  [revise::Bool])

Given test files defined in `source` using the `Test` package, try to load
them by replacing `Test` with `ReTest`, wrapping them in a module `modname`
defined within `parentmodule`. If successful, the newly created module
`modname` is returned and `modname.runtests()` should be callable.

If `source::AbstractString`, then it's interpreted as the top level test file
(possibly including other files), and `modname` defaults to a name based
on `basename(source)`.

If `source::Module`, then it's interpreted as the name of a package, and the
"test/runtests.jl" file from this package is loaded. In this case, `modname`
defaults to `Symbol(source, :Tests)`.

The current procedure is as follows:
1. replace toplevel `using Test` occurrences by `using ReTest` (`using` can
   have multiple arguments);
2. apply recursively these two rules:
   * for all `include`d files, provided the `include` statement is at the toplevel,
     or nested within these toplevel constructs:
     `begin`, `let`, `for`, `while`, `if`, `try`;
   * on the content of all included modules.

When `source` is `Base` or a standard library module, a slightly different
mechanism is used to find test files (which can contain e.g. non-toplevel
`include`s), i.e. `ReTest.hijack_base` is used underneath.

#### `lazy` keyword

The `lazy` keyword specifies whether some toplevel expressions should be skipped:
* `false` means nothing is skipped;
* `true` means toplevel `@test*` macros are removed, as well as those defined
  within these toplevel (but possible nested) blocks:
  `begin`, `let`, `for`, `while`, `if`, `try`;
* `:brutal` means toplevel `@test*` macros are removed, as well as toplevel
  `begin`, `let`, `for` or `if` blocks.

#### `include` keyword

The `include` keyword can help to handle the case where `@testset`s contain
`include` expressions, like in the following example:
```julia
@testset "parent" begin
    @test true
    include("file_with_other_testsets.jl")
end
```

This works well with `Test` because testsets are run immediately, as well as
testsets contained in the included files, which are also recognized as children
of the testset which include them. With `ReTest`, the `include` expressions
would be evaluated only when the parent testsets are run, so that included
testsets are not run themselves, but only "declared".

If the `include` keyword is set to `:static`, `include(...)` expressions are
evaluated when `@testset` expressions containing them are parsed, before
filtering and before testsets are run. Testsets which are declared (within the
same module) as a side effect of `include(...)` are then inserted in place of
the call to `include(...)`.

If the `include` keyword is set to `:outline`, `hijack` inspects topelevel
`@testset` expressions and puts toplevel `include(...)` expressions outside of
the containing testset, and should therefore be evaluated immediately. This is
not ideal, but at least allows `ReTest` to know about all the testsets right
after the call to `hijack`, and to not declare new testsets when parent
testsets are run.

The `:outline` option might be deprecated in the future, and `include=:static`
should generally be preferred. One case where `:outline` might work better is
when the included file defines a submodule: `ReTest` doesn't have the concept
of a nested testset belonging to a different module than the parent testset,
so the best that can be done here is to "outline" such nested testsets; with
`include=:outline`, `hijack` will "process" the content of such submodules
(replace `using Test` by `using ReTest`, etc.), whereas with
`include=:static`, the subdmodules will get defined after `hijack` has
returned (on the first call to `retest` thereafter), so won't be "processed".

#### `revise` keyword

The `revise` keyword specifies whether `Revise` should be used to track
the test files (in particular the testsets). If `true`, `Revise` must
be loaded beforehand in your Julia session. Note that this might be brittle
and not work in all cases. `revise` defaults to `true` when `Revise` is loaded,
and to `false` otherwise.

!!! compat "Julia 1.5"
    This function requires at least Julia 1.5.
"""
function hijack end

# TODO 0.4: remove `testset` kwarg
# TODO: maybe deprecate `include=:outline`?

function hijack(path::AbstractString, modname=nothing; parentmodule::Module=Main,
                lazy=false, revise::Maybe{Bool}=nothing,
                include::Maybe{Symbol}=nothing, testset::Bool=false)

    # do first, to error early if necessary
    Revise = get_revise(revise)

    if modname === nothing
        modname = replace(splitext(basename(path))[1], ['-', '.'] => '_')
    end
    modname = Symbol(modname)

    newmod = @eval parentmodule module $modname end
    populate_mod!(newmod, path; lazy=lazy, include=setinclude(include, testset),
                  Revise=Revise)
    newmod
end

function setinclude(include, testset)
    if testset
        include === nothing || error("cannot specify both `testset` and `include` arguments")
        :outline
    else
        include === nothing || include === :static || include === :outline ||
            error("`include` keyword only accepts `:static` or `:outline` as value")
        include
    end
end

# this is just a work-around for v"1.5", where @__MODULE__ can't be used in
# expressions; root_module[] is set equal to @__MODULE__ within modules
const root_module = Ref{Symbol}()

__init__() = root_module[] = gensym("MODULE")

function populate_mod!(mod::Module, path; lazy, Revise, include)
    lazy ∈ (true, false, :brutal) ||
        throw(ArgumentError("the `lazy` keyword must be `true`, `false` or `:brutal`"))

    files = Revise === nothing ? nothing : Dict(path => mod)
    substitute!(x) = substitute_retest!(x, lazy, include, files)

    @eval mod begin
        using ReTest # for files which don't have `using Test`
        const $(root_module[]) = $mod
        include($substitute!, $path)
        if !@isdefined __revise_mode__
            __revise_mode__ = :eval
        end
    end

    if Revise !== nothing
        revise_track(Revise, files)
    end
end

function revise_track(Revise, files)
    # uniquemod serves in v1.5 to make hijack w/ revise still work in many cases,
    # when there aren't nested submodules/includes
    for (filepath, mod) in files
        if isfile(filepath) # some files might not exist when they are conditionally
                            # included
            Revise.track(mod, filepath)
        end
    end
end

function hijack(packagemod::Module, modname=nothing; parentmodule::Module=Main,
                lazy=false, revise::Maybe{Bool}=nothing,
                include::Maybe{Symbol}=nothing, testset::Bool=false)
    packagepath = pathof(packagemod)
    packagepath === nothing && packagemod !== Base &&
        throw(ArgumentError("$packagemod is not a package"))

    if modname === nothing
        modname = Symbol(packagemod, :Tests)
    end

    if packagemod === Base
        hijack_base(:_Base_tests,
                    base=modname, parentmodule=parentmodule, lazy=lazy, revise=revise)
    elseif startswith(packagepath, Sys.STDLIB) # packagemod is an STDLIB
        hijack_base(string(packagemod), modname,
                    parentmodule=parentmodule, lazy=lazy, revise=revise)
    else
        path = joinpath(dirname(dirname(packagepath)), "test", "runtests.jl")
        hijack(path, modname, parentmodule=parentmodule,
               lazy=lazy, testset=testset, include=include, revise=revise)
    end
end

function substitute_retest!(ex, lazy, include_, files=nothing;
                            ishijack::Bool=true)
    substitute!(x) = substitute_retest!(x, lazy, include_, files, ishijack=ishijack)

    if Meta.isexpr(ex, :using)
        ishijack || return ex
        for used in ex.args
            if Meta.isexpr(used, :., 1) && used.args[1] == :ReTest
                throw(ArgumentError("ReTest is already used"))
            elseif Meta.isexpr(used, :., 1) && used.args[1] == :Test
                used.args[1] = :ReTest
            end
        end
    elseif Meta.isexpr(ex, :call) && ex.args[1] == :include
        if length(ex.args) > 2
            throw(ArgumentError("cannot handle include with two arguments"))
        else
            if files !== nothing
                newfile = ex.args[2]
                ex2 =
                    :(let newfile = $_include_dependency($newfile)
                          haskey($files, newfile) &&
                              error("file belong to multiple modules, currently not supported")
                          include($substitute!, $newfile)
                          $files[newfile] = $(root_module[])
                      end)
                if VERSION >= v"1.6"
                    # v1.5 doesn't play well with @__MODULE__, the let expression
                    # simply... vanishes; so we have to use root_module instead
                    push!(ex2.args[2].args, :(@assert $(root_module[]) == @__MODULE__))
                end
                # TODO: add `copy!(::Expr, ::Expr)` to Base
                ex.head = ex2.head
                copy!(ex.args, ex2.args)
            else
                insert!(ex.args, 2, substitute!)
            end
        end
    elseif Meta.isexpr(ex, :module)
        body = ex.args[3]
        @assert Meta.isexpr(body, :block)
        substitute!.(body.args)
        insert!(body.args, 2, :(const $(root_module[]) = $(ex.args[2])))
        # ^^^ we insert between two LineNumberNode, not sure if this is the way: 1st one
        # seems to relate to the `module` line, while the 2nd relates to next expression
        push!(body.args, :(if !@isdefined __revise_mode__
                               __revise_mode__ = :eval
                           end))
    elseif Meta.isexpr(ex, :macrocall)
        ishijack || return ex
        if lazy != false && ex.args[1] ∈ TEST_MACROS
            empty_expr!(ex)
        elseif include_ !== nothing && ex.args[1] == Symbol("@testset")
            if include_ === :outline
                # we remove `include` expressions and put them out of the `@testset`
                body = ex.args[end]
                if body.head == :for
                    body = body.args[end]
                end
                includes = splice!(body.args, findall(body.args) do x
                                                  Meta.isexpr(x, :call) && x.args[1] == :include
                                              end)
                map!(substitute!, includes, includes)
                ex.head = :block
                newts = Expr(:macrocall, ex.args...)
                push!(empty!(ex.args), newts, includes...)
            else # :static
                pos = ex.args[2] isa LineNumberNode ? 3 : 2
                insert!(ex.args, pos, :(static_include=true))
            end
        end
    elseif ex isa Expr && ex.head ∈ (:block, :let, :for, :while, :if, :try)
        if lazy == :brutal
            empty_expr!(ex)
        else
            beg = ex.head ∈ (:block, :try) ? 1 : 2
            for x in ex.args[beg:end]
                substitute!(x)
            end
        end
    end
    ex
end

function empty_expr!(ex)
    ex.head = :block
    empty!(ex.args)
    ex
end

# from Base, but without touching _track_dependencies
# this allows to get the absolute path from within included files,
# and to update the list of tracked files
# - bare abspath won't work directly, as it's based on CWD
# - @__DIR__ won't work as it will refer to this file if used bare,
#   and if constructed via `Expr(:macrocall, ...)`, a LineNumberNode
#   is required to be passed, which defeats the purpose
function _include_dependency(_path::AbstractString)
    prev = Base.source_path(nothing)
    if prev === nothing
        abspath(_path)
    else
        normpath(joinpath(dirname(prev), _path))
    end
end

const TEST_MACROS = Symbol.(["@test", "@test_throws", "@test_broken", "@test_skip",
                             "@test_warn", "@test_nowarn", "@test_logs",
                             "@test_deprecated", "@inferred"])


"""
    hijack_base(tests, [modname];
                parentmodule::Module=Main, lazy=false, [revise::Bool])

Similar to `ReTest.hijack`, but specifically for `Base` and stdlib tests.
`tests` speficies which test files should be loaded, in the exact same format
as `Base.runtests` (i.e. it uses the same `choosetests` function to
select the tests).

Tests corresponding to a "test/[somedir/]sometest.jl" file are loaded in the
`BaseTests.[somedir.]sometest` module (if `sometest` is defined in `Base`,
then `sometest_` is used instead).

Tests corresponding to a standard library `Lib` are loaded in the
`StdLibTests.Lib_` module. When there are "testgroups", submodules
are created accordingly.

If `modname` is specified (experimental), this will be the name of the module
in which testsets are defined; passing `modname` is allowed only when all
the loaded tests would otherwise be defined in the same second top-most
module, the one under `BaseTests` or `StdLibTests` (e.g. `somedir` if any,
or `sometest` otherwise, or `Lib_`). This unique module is then named `modname`,
and not enclosed within `BaseTests` or `StdLibTests`.

The `lazy` and `revise` keywords have the same meaning as in [`ReTest.hijack`](@ref).
Depending on the value of `lazy`, some test files are skipped when they
are known to fail.

!!! compat "Julia 1.5"
    This function requires at least Julia 1.5.
"""
function hijack_base(tests, modname=nothing; parentmodule::Module=Main, lazy=false,
                     base=:BaseTests, stdlib=:StdLibTests, revise::Maybe{Bool}=nothing)

    Revise = get_revise(revise)
    if isa(tests, AbstractString)
        tests = split(tests)
    end

    tests, = ChooseTests.call_choosetests(tests) # TODO: handle other return values?
    allcomponents = (components -> Symbol.(components)).(split.(tests, '/'))

    if modname !== nothing
        modname = Symbol(modname)
        allequal(components[1] for components in allcomponents) ||
            throw(ArgumentError("can't specify `modname` for multiple test roots"))
        # if modname was passed, it seems nicer to have it fresh, otherwise it might
        # contain left-overs from previous incantations (and potentially lead to
        # a series of "WARNING: replacing module submodule_i")
        @eval parentmodule module $modname end
    end

    for (test, components) in zip(tests, allcomponents)
        if test ∈ ChooseTests.BLACKLIST ||
            lazy == true && test ∈ ChooseTests.BLACKLIST_SOFT ||
            lazy == :brutal && test ∈ ChooseTests.BLACKLIST_BRUTAL
            @warn "skipping \"$test\" test (incompatible with ReTest)"
            continue
        end
        if test ∈ ["show"] && !isdefined(Main, :Distributed)
            @eval Main using Distributed
        end

        if modname === nothing
            if string(components[1]) in ChooseTests.STDLIBS
                # it's an stdlib Lib, make toplevel modules StdLibTests/Lib_
                components[1] = Symbol(components[1], :_)
                pushfirst!(components, Symbol(stdlib))
            else
                # it's a Base test, use BaseTests as toplevel module
                pushfirst!(components, Symbol(base))
            end
        else
            components[1] = modname
        end

        mod = parentmodule
        for (ith, comp) in enumerate(components)
            if isdefined(Base, comp) || comp ∈ ChooseTests.EPONYM_TESTS
                # e.g. `tuple`, collision betwen the tuple function and test/tuple.jl
                comp = Symbol(comp, :_)
            end
            if isdefined(mod, comp) && ith != length(components) ||
                    modname !== nothing && ith == 1 # module already freshly created
                mod = getfield(mod, comp)
            else
                # we always re-eval leaf-modules
                mod = @eval mod module $comp end
            end
        end

        @eval mod begin
            using Random
        end
        populate_mod!(mod, ChooseTests.test_path(test), lazy=lazy, Revise=Revise, testset=false)
    end
end

get_revise(revise) =
    if revise === true || revise === nothing && VERSION >= v"1.5"
        Revise = get(Base.loaded_modules, revise_pkgid(), nothing)
        Revise === nothing && revise === true &&
            error("Revise is not loaded")
        Revise
    end



module ChooseTests
# choosetests.jl requires the Sockets package, which is why it's a ReTest dependency

const BASETESTPATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")

# we include "choosetests.jl" on the fly, because the computation of the
# STDLIBS constant in this file currently doubles the load time of ReTest
# (quite negligible though, takes about 0.005s)

function call_choosetests(choices)
    isdefined(ChooseTests, :STDLIBS) ||
        include(joinpath(BASETESTPATH, "choosetests.jl"))

    if choices === :_Base_tests
        # we compute choices lazily, as in recent Julia versions, TESTNAMES is defined
        # globally in choosetests.jl, and therefore defined only after the include above
        choices = filter(x -> x != "stdlib", TESTNAMES)
    end
    Base.invokelatest(choosetests, choices)
end

# adapted from julia/tests/runtests.jl
function test_path(test)
    t = split(test, '/')

    if t[1] in STDLIBS
        if length(t) == 2
            return joinpath(STDLIB_DIR, t[1], "test", "$(t[2]).jl")
        else
            return joinpath(STDLIB_DIR, t[1], "test", "runtests.jl")
        end
    else
        return joinpath(BASETESTPATH, "$test.jl")
    end
end

if VERSION < v"1.8.0-DEV.34"
    const TESTNAMES = [
        "subarray", "core", "compiler", "worlds",
        "keywordargs", "numbers", "subtype",
        "char", "strings", "triplequote", "unicode", "intrinsics",
        "dict", "hashing", "iobuffer", "staged", "offsetarray",
        "arrayops", "tuple", "reduce", "reducedim", "abstractarray",
        "intfuncs", "simdloop", "vecelement", "rational",
        "bitarray", "copy", "math", "fastmath", "functional", "iterators",
        "operators", "ordering", "path", "ccall", "parse", "loading", "gmp",
        "sorting", "spawn", "backtrace", "exceptions",
        "file", "read", "version", "namedtuple",
        "mpfr", "broadcast", "complex",
        "floatapprox", "stdlib", "reflection", "regex", "float16",
        "combinatorics", "sysinfo", "env", "rounding", "ranges", "mod2pi",
        "euler", "show", "client",
        "errorshow", "sets", "goto", "llvmcall", "llvmcall2", "ryu",
        "some", "meta", "stacktraces", "docs",
        "misc", "threads", "stress", "binaryplatforms", "atexit",
        "enums", "cmdlineargs", "int", "interpreter",
        "checked", "bitset", "floatfuncs", "precompile",
        "boundscheck", "error", "ambiguous", "cartesian", "osutils",
        "channels", "iostream", "secretbuffer", "specificity",
        "reinterpretarray", "syntax", "corelogging", "missing", "asyncmap",
        "smallarrayshrink", "opaque_closure"
    ]
end

const BLACKLIST = [
    # failing at load time (in hijack_base)
    "backtrace", "misc", "threads", "cmdlineargs","boundscheck",
    "SharedArrays", # problem with workers (should be sortable out)
    "Test",
    # failing at runtime (in retest)
    "precompile", # no toplevel testset, just one with interpolated subject
]

const BLACKLIST_BRUTAL = split("core worlds bitarray file reflection errorshow asyncmap
                                opaque_closure") # llvmcall: doesn't work if loaded twice

const BLACKLIST_SOFT = split("core worlds offsetarray spawn file errorshow")

# file names which define a global variable of the same name as the file
const EPONYM_TESTS = [:worlds, :file]

end # ChooseTests
