using ReTest # useful when setup.jl included separately

module Trace
export trace
using ReTest: @test

const RUN = []

function trace(x)
    push!(RUN, x)
    @test true
end
end

function check(x...; runtests=false, verbose=true, stats=false, dry=false, strict::Bool=true,
               recursive=true)
    args = x[1:end-1]
    expected = x[end]
    if expected isa AbstractString
        expected = split(x[end])
    end

    empty!(Trace.RUN)
    if runtests
        getfield(args[1], :runtests)(args[2:end]...; verbose=verbose, stats=stats, dry=dry,
                                     strict=strict, recursive=recursive)
    else
        retest(args...; verbose=verbose, stats=stats, dry=dry, strict=strict,
               recursive=recursive)
    end
    @test Trace.RUN == expected
end

macro chapter(title, x)
    title = string(title)

    if isempty(ARGS) || any(pat -> occursin(Regex(pat, "i"), title), ARGS)
        printstyled("\n\n", rpad("## $title #", 78, '#'), "\n\n", bold=true, color=:cyan)
        esc(x)
    end
end
