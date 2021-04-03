"""
    ReTest.hijack(source, [modname];
                  parentmodule::Module=Main, lazy=false, revise::Bool=false)

Given test files defined in `source` using the `Test` package, try to load
them by replacing `Test` with `ReTest`, wrapping them in a module `modname`
defined within `parentmodule`. If successful, the newly created module
`modname` is returned and `modname.runtests()` should be callable.

If `source::AbstractString`, then it's interpreted as the top level test file
(possibly including other files), and `modname` defaults to an arbitrary name.

If `source::Module`, then it's interpreted as the name of a package, and the
"test/runtests.jl" file from this package is loaded. In this case, `modname`
defaults to `Symbol(source, :Tests)`.

The current procedure is as follows:
1. replace toplevel `using Test` occurrences by `using ReTest` (`using` can
   have multiple arguments);
2. apply recursively these two rules for all `include`d files, provided
   the `include` statement is at the toplevel, and on the content of
   all modules.

When `source` is `Base` or a standard library module, a slightly different
mechanism is used to find test files (which can contain e.g. non-toplevel
`include`s), i.e. `ReTest.hijack_base` is used underneath.

The `lazy` keyword specifies whether some toplevel expressions should be skipped:
* `false` means nothing is skipped;
* `true` means toplevel `@test*` macros are removed, as well as those defined
  within these toplevel (but possible nested) blocks: `begin`, `let`, `for`, `if`;
* `:brutal` means toplevel `@test*` macros are removed, as well as toplevel
  `begin`, `let`, `for` or `if` blocks.

The `revise` keyword specifies whether `Revise` should be used to track
the test files (in particular the testsets). If `true`, `Revise` must
be loaded beforehand in your Julia session.

!!! compat "Julia 1.5"
    This function requires at least Julia 1.5.
"""
function hijack end

function hijack(path::AbstractString, modname=nothing; parentmodule::Module=Main,
                                                       lazy=false, revise::Bool=false)
    # do first, to error early if necessary
    Revise = get_revise(revise)

    if modname === nothing
        modname = :TestMod
        i = 1
        while hasproperty(parentmodule, modname)
            modname = Symbol(:TestMod, i)
            i += 1
        end
    end
    modname = Symbol(modname)

    newmod = @eval parentmodule module $modname end
    populate_mod!(newmod, path; lazy=lazy, Revise=Revise)
    newmod
end

function populate_mod!(mod, path; lazy, Revise)
    files = Any[path]
    substitue!(x) = substitue_retest!(x, lazy, files, dirname(path))

    @eval mod begin
        using ReTest # for files which don't have `using Test`
        include($substitue!, $path)
    end

    if Revise !== nothing
        filepaths = @eval mod begin
            __revise_mode__ = :eval
            [$(files...)]
        end
        for filepath in filepaths
            Revise.track(mod, filepath)
        end
    end
end

function hijack(packagemod::Module, modname=nothing; parentmodule::Module=Main,
                                                     lazy=false, revise::Bool=false)
    packagepath = pathof(packagemod)
    packagepath === nothing && packagemod !== Base &&
        throw(ArgumentError("$packagemod is not a package"))

    if modname === nothing
        modname = Symbol(packagemod, :Tests)
    end

    if packagemod === Base
        hijack_base(ChooseTests.BASETESTS,
                    base=modname, parentmodule=parentmodule, lazy=lazy, revise=revise)
    elseif startswith(packagepath, Sys.STDLIB) # packagemod is an STDLIB
        hijack_base(string(packagemod), modname,
                    parentmodule=parentmodule, lazy=lazy, revise=revise)
    else
        path = joinpath(dirname(dirname(packagepath)), "test", "runtests.jl")
        hijack(path, modname, parentmodule=parentmodule, lazy=lazy, revise=revise)
    end
end

function substitue_retest!(ex, lazy, files=nothing, root=nothing)
    substitue!(x) = substitue_retest!(x, lazy, files, root)

    if Meta.isexpr(ex, :using)
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
                newfile = :(joinpath($root, $(ex.args[2])))
                push!(files, newfile)
                root = :(dirname($newfile))
            end
            insert!(ex.args, 2, substitue!)
        end
    elseif Meta.isexpr(ex, :module)
        @assert Meta.isexpr(ex.args[3], :block)
        substitue!.(ex.args[3].args)
    else
        filter_toplevel!(ex, lazy)
    end
    ex
end

function empty_expr!(ex)
    ex.head = :block
    empty!(ex.args)
    ex
end

const TEST_MACROS = Symbol.(["@test", "@test_throws", "@test_broken", "@test_skip",
                             "@test_warn", "@test_nowarn", "@test_logs",
                             "@test_deprecated", "@inferred"])

function filter_toplevel!(ex, lazy)
    lazy != false && ex isa Expr || return ex

    if ex.head == :macrocall &&
        ex.args[1] ∈ TEST_MACROS
        empty_expr!(ex)
    elseif ex.head ∈ (:block, :let, :for, :if)
        if lazy == :brutal
            empty_expr!(ex)
        else
            beg = ex.head == :block ? 1 : 2
            for x in ex.args[beg:end]
                filter_toplevel!(x, lazy)
            end
        end
    end
    ex
end

"""
    hijack_base(tests, [modname];
                parentmodule::Module=Main, lazy=false, revise::Bool=false)

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
                     base=:BaseTests, stdlib=:StdLibTests, revise::Bool=false)

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
        populate_mod!(mod, ChooseTests.test_path(test), lazy=lazy, Revise=Revise)
    end
end

get_revise(revise) =
    if revise
        Revise = get(Base.loaded_modules, revise_pkgid(), nothing)
        Revise === nothing &&
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

const BASETESTS = filter(x -> x != "stdlib", TESTNAMES)

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
