const _pass = :__pass__
const _fail = :__fail__

# marks contains at most one of _pass, _fail

# return true (pass), false (fail), or nothing (unrun)
function pastresult(marks::Dict, subject)
    ms = get(marks, subject, Symbol())
    sym::Symbol = ms isa Symbol ? ms : isempty(ms) ? Symbol() : ms[1]
    sym === _pass ? true : sym === _fail ? false : nothing
end

function setresult!(marks::Dict, subject, success::Bool)
    ms = get(marks, subject, Symbol())
    res = success ? _pass : _fail
    if ms isa Symbol
        # ms could be Symbol() from get's default or delmark!
        if ms ∈ (_pass, _fail, Symbol())
            marks[subject] = res
        else
            # res always in first position
            marks[subject] = [res, ms]
        end
    else # ms isa Vector
        if !isempty(ms) && ms[1] ∈ (_pass, _fail)
            ms[1] = res
        else
            pushfirst!(ms, res)
        end
    end
end

function markiter(marks, subject, skipres::Bool)
    ms = get(marks, subject, Symbol())
    if ms isa Symbol
        if ms === Symbol() || skipres && ms ∈ (_pass, _fail)
            ()
        else
            (ms,)
        end
    else
        if skipres
            Iterators.filter(m -> m ∉ (_pass, _fail), ms)
        else
            ms
        end
    end
end

function addmark!(marks, subject, m::Symbol)
    ms = get(marks, subject, Symbol())
    if ms isa Symbol
        if ms === m
            false
        elseif ms === Symbol()
            marks[subject] = m
            true
        else
            marks[subject] = [ms, m]
            true
        end
    elseif findfirst(==(m), ms) === nothing
        push!(ms, m)
        true
    else
        false
    end
end

function delmark!(marks, subject, m::Symbol)
    ms = get(marks, subject, Symbol())
    if ms isa Symbol
        if ms === m
            marks[subject] = Symbol()
        end
    else
        p = findfirst(==(m), ms)
        if p !== nothing
            deleteat!(ms, p)
        end
    end
    nothing
end

function hasmark(marks, subject, m::Symbol)
    ms = get(marks, subject, Symbol())
    if ms isa Symbol
        ms === m
    else
        m ∈ ms
    end
end
