"""
    ReTest.hijack(source, [modname]; parentmodule::Module=Main)

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
"""
function hijack end

function hijack(path::AbstractString, modname=nothing; parentmodule::Module=Main)
    if modname === nothing
        modname = :TestMod
        i = 1
        while hasproperty(parentmodule, modname)
            modname = Symbol(:TestMod, i)
            i += 1
        end
    end
    modname = Symbol(modname)

    newmod = @eval parentmodule module $modname
                                    using ReTest # for files which don't have `using Test`
                                    include($substitue_retest!, $path)
                                end
    newmod
end

function hijack(packagemod::Module, modname=nothing; parentmodule::Module=Main)
    path = joinpath(dirname(dirname(pathof(packagemod))), "test", "runtests.jl")
    if modname === nothing
        modname = Symbol(packagemod, :Tests)
    end
    hijack(path, modname, parentmodule=parentmodule)
end

function substitue_retest!(ex)
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
            insert!(ex.args, 2, substitue_retest!)
        end
    elseif Meta.isexpr(ex, :module)
        @assert Meta.isexpr(ex.args[3], :block)
        substitue_retest!.(ex.args[3].args)
    end
    ex
end

const BASETESTS_BLACKLIST = ["backtrace", "misc", "threads", "cmdlineargs","boundscheck",
                             "SharedArrays", "Test"]
# SharedArrays: problem with workers (should be sortable out)

"""
    hijack_base(tests; parentmodule::Module=Main)

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
"""
function hijack_base(tests; parentmodule::Module=Main)
    if isa(tests, AbstractString)
        tests = split(tests)
    end

    tests, = ChooseTests.call_choosetests(tests) # TODO: handle other return values?

    for test in tests
        if test ∈ BASETESTS_BLACKLIST
            @warn "skipping \"$test\" test (incompatible with ReTest)"
            continue
        end
        if test ∈ ["show"] && !isdefined(Main, :Distributed)
            @eval Main using Distributed
        end

        components = Symbol.(split(test, '/'))
        if string(components[1]) in ChooseTests.STDLIBS
            # it's an stdlib Lib, make toplevel modules StdLibTests/Lib_
            components[1] = Symbol(components[1], :_)
            pushfirst!(components, :StdLibTests)
        else
            # it's a Base test, use BaseTests as toplevel module
            pushfirst!(components, :BaseTests)
        end

        mod = parentmodule
        for (ith, comp) in enumerate(components)
            if isdefined(Base, comp)
                # e.g. `tuple`, collision betwen the tuple function and test/tuple.jl
                comp = Symbol(comp, :_)
            end

            if isdefined(mod, comp) && ith != length(components)
                mod = getfield(mod, comp)
            else
                # we always re-eval leaf-modules
                mod = @eval mod module $comp end
            end
        end

        @eval mod begin
                      using ReTest, Random
                      include($substitue_retest!, $(ChooseTests.test_path(test)))
                  end
    end
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

end # ChooseTests
