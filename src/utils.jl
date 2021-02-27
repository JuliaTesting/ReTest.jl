issubmodule(m::Module, s) = s isa Module && parentmodule(s) == m && m != s

function submodules(m::Module)
    symbols = Core.eval.(Ref(m), filter!(y -> isdefined(m, y), names(m, all=true)))
    filter!(x -> issubmodule(m, x), symbols)
end

# list of recursive submodules of m, including m itself
function recsubmodules(m::Module)
    subs = submodules(m)
    if isempty(subs)
        recs = subs
    else
        recs = mapreduce(recsubmodules, vcat, subs)
    end
    pushfirst!(recs, m)
    recs
end

function allequal(xs)
    local val
    for x in xs
        if !@isdefined(val)
            val = x
        else
            isequal(val, x) || return false
        end
    end
    true
end
