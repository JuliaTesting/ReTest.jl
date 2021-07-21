struct Marks
    # Union to avoid creating a vector in most cases
    soft::Dict{String, Union{Symbol, Vector{Symbol}}} # TODO: should be a MultiDict
    hard::Vector{Symbol} # attached to all instances of a @testset
end

Marks() = Marks(Dict{String, Union{Symbol, Vector{Symbol}}}(), Symbol[])

const _pass = :__pass__
const _fail = :__fail__

# marks contains at most one of _pass, _fail

# return true (pass), false (fail), or nothing (unrun)
function pastresult(marks::Marks, subject)
    ms = get(marks.soft, subject, Symbol())
    sym::Symbol = ms isa Symbol ? ms : isempty(ms) ? Symbol() : ms[1]
    sym === _pass ? true : sym === _fail ? false : nothing
end

function setresult!(marks::Marks, subject, success::Bool)
    soft = marks.soft
    ms = get(soft, subject, Symbol())
    res = success ? _pass : _fail
    if ms isa Symbol
        # ms could be Symbol() from get's default or delmark!
        if ms ∈ (_pass, _fail, Symbol())
            soft[subject] = res
        else
            # res always in first position
            soft[subject] = [res, ms]
        end
    else # ms isa Vector
        if !isempty(ms) && ms[1] ∈ (_pass, _fail)
            ms[1] = res
        else
            pushfirst!(ms, res)
        end
    end
end

function markiter(marks::Marks, subject, skipres::Bool)
    ms = get(marks.soft, subject, Symbol())
    if ms isa Symbol
        if ms === Symbol() || skipres && ms ∈ (_pass, _fail)
            marks.hard
        else
            Iterators.flatten((marks.hard, (ms,)))
        end
    else
        Iterators.flatten((marks.hard,
                           if skipres
                               Iterators.filter(m -> m ∉ (_pass, _fail), ms)
                           else
                               ms
                           end))
    end
end

function addmark!(marks::Marks, subject, m::Symbol)
    m ∈ marks.hard && return false
    soft = marks.soft
    ms = get(soft, subject, Symbol())
    if ms isa Symbol
        if ms === m
            false
        elseif ms === Symbol()
            soft[subject] = m
            true
        else
            soft[subject] = [ms, m]
            true
        end
    elseif m ∉ ms
        push!(ms, m)
        true
    else
        false
    end
end

# return whether warning is issued
function delmark!(marks::Marks, subject, m::Symbol, warned::Bool=false)
    if m ∈ marks.hard && !warned
        @warn "cannot remove statically attached label (@testset :$m ...)"
        return true
    end
    soft = marks.soft
    ms = get(soft, subject, Symbol())
    if ms isa Symbol
        if ms === m
            soft[subject] = Symbol()
        end
    else
        p = findfirst(==(m), ms)
        if p !== nothing
            deleteat!(ms, p)
        end
    end
    false
end

function hasmark(marks::Marks, subject, m::Symbol)
    m ∈ marks.hard && return true
    ms = get(marks.soft, subject, Symbol())
    if ms isa Symbol
        ms === m
    else
        m ∈ ms
    end
end
