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

function check(x...; runtests=false, output::Union{Nothing,String}=nothing,
               verbose=true, stats=false, dry=false, strict::Bool=true,
               recursive=true, static=nothing, id=nothing, load=false)
    @assert !(runtests & (output !== nothing)) "unimplemented"
    args = x[1:end-1]
    expected = x[end]
    if expected isa AbstractString
        expected = split(x[end])
    end

    empty!(Trace.RUN)
    if runtests
        getfield(args[1], :runtests)(args[2:end]...; verbose=verbose, stats=stats, dry=dry,
                                     strict=strict, recursive=recursive, static=static,
                                     id=id, load=load)
    elseif output === nothing
        retest(args...; verbose=verbose, stats=stats, dry=dry, strict=strict,
               recursive=recursive, static=static, id=id, load=load)
    else
        mktemp() do path, io
            redirect_stdout(io) do
                retest(args...; verbose=verbose, stats=stats, dry=dry, strict=strict,
                       recursive=recursive, static=static, id=id, load=load)
            end
            seekstart(io)
            printed = join(map(rstrip, split(readchomp(io), '\n')), '\n')
            output = join(map(rstrip, split(chomp(output), '\n')), '\n')
            @test printed == output
        end
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
