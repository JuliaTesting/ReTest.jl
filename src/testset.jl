module Testset

using Test: DefaultTestSet, Error, Test, _check_testset, finish, get_testset,
            get_testset_depth, parse_testset_args, pop_testset, push_testset, record

import Random

import InlineTest: @testset

default_rng() = isdefined(Random, :default_rng) ?
    Random.default_rng() :
    Random.GLOBAL_RNG

function get_testset_string(remove_last=false)
    testsets = get(task_local_storage(), :__BASETESTNEXT__, Test.AbstractTestSet[])
    join('/' * ts.description for ts in (remove_last ? testsets[1:end-1] : testsets))
end

# non-inline testset with regex filtering support
macro testset(isfinal::Bool, rx::Regex, desc::String, body)
    Testset.testset_beginend(isfinal, rx, desc, body, __source__)
end

macro testset(isfinal::Bool, rx::Regex, desc::Union{String,Expr},
              loopiter, loopvals,
              body)
    Testset.testset_forloop(isfinal, rx, desc, loopiter, loopvals, body, __source__)
end

"""
Generate the code for a `@testset` with a `begin`/`end` argument
"""
function testset_beginend(isfinal::Bool, rx::Regex, desc::String, tests, source)
    testsettype = :(get_testset_depth() == 0 ? DefaultTestSet : typeof(get_testset()))
    # Generate a block of code that initializes a new testset, adds
    # it to the task local storage, evaluates the test(s), before
    # finally removing the testset and giving it a chance to take
    # action (such as reporting the results)
    ex = quote
        local current_str
        if $isfinal
            current_str = string(get_testset_string(), '/', $desc)
        end
        if !$isfinal || occursin($rx, current_str)
            _check_testset($testsettype, $(QuoteNode(testsettype.args[1])))
            local ret
            local ts = $(testsettype)($desc)
            push_testset(ts)
            # we reproduce the logic of guardseed, but this function
            # cannot be used as it changes slightly the semantic of @testset,
            # by wrapping the body in a function
            local RNG = default_rng()
            local oldrng = copy(RNG)
            try
                # RNG is re-seeded with its own seed to ease reproduce a failed test
                Random.seed!(RNG.seed)
                let
                    $(esc(tests))
                end
            catch err
                err isa InterruptException && rethrow()
                # something in the test block threw an error. Count that as an
                # error in this test set
                record(ts, Error(:nontest_error, Expr(:tuple), err, Base.catch_stack(), $(QuoteNode(source))))
            finally
                copy!(RNG, oldrng)
                pop_testset()
                ret = finish(ts)
            end
            ret
        end
    end
    # preserve outer location if possible
    if tests isa Expr && tests.head === :block && !isempty(tests.args) && tests.args[1] isa LineNumberNode
        ex = Expr(:block, tests.args[1], ex)
    end
    return ex
end

"""
Generate the code for a `@testset` with a `for` loop argument
"""
function testset_forloop(isfinal::Bool, rx::Regex, desc::Union{String,Expr},
                         loopiter, loopvals,
                         tests, source)

    # Pull out the loop variables. We might need them for generating the
    # description and we'll definitely need them for generating the
    # comprehension expression at the end
    loopvars = Expr[Expr(:(=), loopiter, loopvals)]

    testsettype = :(get_testset_depth() == 0 ? DefaultTestSet : typeof(get_testset()))

    blk = quote
        local current_str
        if $isfinal
            current_str = string(get_testset_string(!first_iteration), '/', $(esc(desc)))
        end
        if !$isfinal || occursin($rx, current_str)
            _check_testset($testsettype, $(QuoteNode(testsettype.args[1])))
            # Trick to handle `break` and `continue` in the test code before
            # they can be handled properly by `finally` lowering.
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts))
                # it's 1000 times faster to copy from tmprng rather than calling Random.seed!
                copy!(RNG, tmprng)
            end
            ts = $(testsettype)($(esc(desc)))
            push_testset(ts)
            first_iteration = false
            try
                $(esc(tests))
            catch err
                err isa InterruptException && rethrow()
                # Something in the test block threw an error. Count that as an
                # error in this test set
                record(ts, Error(:nontest_error, Expr(:tuple), err, Base.catch_stack(), $(QuoteNode(source))))
            end
        end
    end
    quote
        local arr = Vector{Any}()
        local first_iteration = true
        local ts
        local RNG = default_rng()
        local oldrng = copy(RNG)
        Random.seed!(RNG.seed)
        local tmprng = copy(RNG)
        try
            let
                $(Expr(:for, Expr(:block, [esc(v) for v in loopvars]...), blk))
            end
        finally
            # Handle `return` in test body
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts))
            end
            copy!(RNG, oldrng)
        end
        arr
    end
end

end # module
