Maybe{T} = Union{T,Nothing}

# test if a module or one of its parents was replaced
function is_replaced(mod::Module)
    par = parentmodule(mod)
    while par != mod
        invokelatest(getglobal, par, nameof(mod)) != mod && return true
        mod = par
        par = parentmodule(par)
    end
    false
end

# list of recursive parent modules of mod, starting with mod
function parentmodules(mod::Module)
    pars = [mod]
    while true
        par = parentmodule(mod)
        par == mod && return pars
        push!(pars, par)
        mod = par
    end
end
