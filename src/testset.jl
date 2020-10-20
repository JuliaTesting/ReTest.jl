module Testset

include("regex.jl")

using Test: DefaultTestSet, Error, Test, _check_testset, finish, get_testset,
            get_testset_depth, parse_testset_args, pop_testset, push_testset, record

using Random: Random, default_rng

const REGEX = Ref{Symbol}()
const FINAL = Ref{Symbol}(:__FINAL__) # must have a value at compile time for InlineTestTest

function __init__()
    REGEX[] = gensym()
    FINAL[] = gensym()
end

function get_testset_string(remove_last=false)
    testsets = get(task_local_storage(), :__BASETESTNEXT__, Test.AbstractTestSet[])
    join('/' * ts.description for ts in (remove_last ? testsets[1:end-1] : testsets))
end

macro testsetr(args...) # testset wirh [r]egex filtering support
    isempty(args) && error("No arguments to @testset")

    tests = args[end]

    # Determine if a single block or for-loop style
    if !isa(tests,Expr) || (tests.head !== :for && tests.head !== :block)
        error("Expected begin/end block or for loop as argument to @testset")
    end

    if tests.head === :for
        return testset_forloop(args, tests, __source__)
    else
        return testset_beginend(args, tests, __source__)
    end
end

"""
Generate the code for a `@testset` with a `begin`/`end` argument
"""
function testset_beginend(args, tests, source)
    desc, testsettype, options = parse_testset_args(args[1:end-1])
    if desc === nothing
        desc = "test set"
    end
    # If we're at the top level we'll default to DefaultTestSet. Otherwise
    # default to the type of the parent testset
    if testsettype === nothing
        testsettype = :(get_testset_depth() == 0 ? DefaultTestSet : typeof(get_testset()))
    end

    # Generate a block of code that initializes a new testset, adds
    # it to the task local storage, evaluates the test(s), before
    # finally removing the testset and giving it a chance to take
    # action (such as reporting the results)
    ex = quote
        local current_str = get_testset_string() * '/' * $desc
        local rx = $(esc(REGEX[]))[1 + $(esc(FINAL[]))]
        if partialoccursin(rx, current_str)
            _check_testset($testsettype, $(QuoteNode(testsettype.args[1])))
            local ret
            local ts = $(testsettype)($desc; $options...)
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
function testset_forloop(args, testloop, source)
    # Pull out the loop variables. We might need them for generating the
    # description and we'll definitely need them for generating the
    # comprehension expression at the end
    loopvars = Expr[]
    if testloop.args[1].head === :(=)
        push!(loopvars, testloop.args[1])
    elseif testloop.args[1].head === :block
        for loopvar in testloop.args[1].args
            push!(loopvars, loopvar)
        end
    else
        error("Unexpected argument to @testset")
    end

    desc, testsettype, options = parse_testset_args(args[1:end-1])

    if desc === nothing
        # No description provided. Generate from the loop variable names
        v = loopvars[1].args[1]
        desc = Expr(:string, "$v = ", esc(v)) # first variable
        for l = loopvars[2:end]
            v = l.args[1]
            push!(desc.args, ", $v = ")
            push!(desc.args, esc(v))
        end
    end

    if testsettype === nothing
        testsettype = :(get_testset_depth() == 0 ? DefaultTestSet : typeof(get_testset()))
    end

    # Uses a similar block as for `@testset`, except that it is
    # wrapped in the outer loop provided by the user
    tests = testloop.args[2]
    blk = quote
        local current_str = get_testset_string(!first_iteration) * '/' * $desc
        local rx = $(esc(REGEX[]))[1 + $(esc(FINAL[]))]
        if partialoccursin(rx, current_str)
            _check_testset($testsettype, $(QuoteNode(testsettype.args[1])))
            # Trick to handle `break` and `continue` in the test code before
            # they can be handled properly by `finally` lowering.
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts))
                # it's 1000 times faster to copy from tmprng rather than calling Random.seed!
                copy!(RNG, tmprng)
            end
            ts = $(testsettype)($desc; $options...)
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
