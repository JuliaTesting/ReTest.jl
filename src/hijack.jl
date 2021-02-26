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
