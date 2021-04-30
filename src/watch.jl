"""
    ReTest.watch(args...; kwargs...)

Run `retest(args...; kwargs...)` repeatedly each time `Revise` detects file updates.
`Revise` must be loaded beforehand in your Julia session.

!!! warning
    This experimental function is not tested and is currently very basic.
"""
function watch(args...; kwargs...)
    Revise = get_revise(true)
    logs = Revise.debug_logger(; min_level=Revise.Debug).logs
    local len
    try
        while true
            if !@isdefined(len) || length(logs) > len
                if !@isdefined(len) ||
                        any(x -> x.message != "LineOffset", @view(logs[len+1:end]))
                    try
                        retest(args...; kwargs...)
                    catch ex
                        if ex isa InterruptException
                            break
                        end
                    end
                    printstyled("\nwatching...\n", bold=true, color=:cyan)
                end
                len = length(logs)
            else
                sleep(0.1)
                Revise.revise()
            end
        end
    catch ex
        ex isa InterruptException || rethrow()
    end
    nothing
end
