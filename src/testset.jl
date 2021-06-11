module Testset

using Test: AbstractTestSet, Broken, DefaultTestSet, Error, Fail, Pass, Test,
            TestSetException, get_testset, get_testset_depth,
            parse_testset_args, pop_testset, push_testset

import Test: finish, record

import Random

using Printf: @sprintf

using Distributed: myid, nworkers

import InlineTest: @testset

using ..ReTest: Pattern, matches

# mostly copied from Test stdlib
# changed from Test: pass nothing as file in ip_has_file_and_func
#-----------------------------------------------------------------------

# (this has to be copied from Test, because `@__FILE__` is hardcoded)

# Backtrace utility functions
function ip_has_file_and_func(ip, file, funcs)
    return any(fr -> ((file === nothing || string(fr.file) == file) &&
                      fr.func in funcs),
               StackTraces.lookup(ip))
end

function scrub_backtrace(bt)
    do_test_ind = findfirst(ip -> ip_has_file_and_func(ip, nothing,
                                                       (:do_test, :do_test_throws)), bt)
    if do_test_ind !== nothing && length(bt) > do_test_ind
        bt = bt[do_test_ind + 1:end]
    end
    name_ind = findfirst(ip -> ip_has_file_and_func(ip, @__FILE__, (Symbol("macro expansion"),)), bt)
    if name_ind !== nothing && length(bt) != 0
        bt = bt[1:name_ind]
    end
    return bt
end

function scrub_exc_stack(stack)
    return Any[ (x[1], scrub_backtrace(x[2])) for x in stack ]
end

mutable struct Format
    stats::Bool
    desc_align::Int
    pass_width::Int
    fail_width::Int
    error_width::Int
    broken_width::Int
    total_width::Int
end

Format(stats, desc_align) = Format(stats, desc_align, 0, 0, 0,0 ,0)

mutable struct ReTestSet <: AbstractTestSet
    mod::Module # enclosing module
    parent::Union{Nothing,ReTestSet}
    description::String
    subject::String
    id::Int64
    overall::Bool # TODO: could be conveyed by having self.mod == ""
    results::Vector
    n_passed::Int
    anynonpass::Bool
    verbose::Bool
    timed::NamedTuple
    exception::Union{TestSetException,Nothing}
end

function ReTestSet(mod, desc::String, id::Integer=0;
                   overall=false, verbose=true, parent=nothing)
    parentsubj = parent === nothing ? "" : parent.subject
    subject = string(parentsubj, '/', desc)
    ReTestSet(mod, parent, desc, subject, id, overall, [], 0, false,
              verbose, NamedTuple(), nothing)
end

# For a non-passed result, simply store the result
record(ts::ReTestSet, t::Union{Broken,Fail,Error}) = (push!(ts.results, t); t)
# For a passed result, do not store the result since it uses a lot of memory
record(ts::ReTestSet, t::Pass) = (ts.n_passed += 1; t)

# When a ReTestSet finishes, it records itself to its parent
# testset, if there is one. This allows for recursive printing of
# the results at the end of the tests
record(ts::ReTestSet, t::AbstractTestSet) = push!(ts.results, t)

function print_test_errors(ts::ReTestSet)
    for t in ts.results
        if isa(t, Error) || isa(t, Fail)
            printstyled(ts.description, ": ", color=:white)

            # don't print for interrupted tests
            if t isa Fail || t.test_type !== :test_interrupted
                show(t)
            end
            if t isa Fail # if not gets printed in the show method
                # Base.show_backtrace(stdout, scrub_backtrace(backtrace()))
            end
            println()
        elseif isa(t, ReTestSet)
            print_test_errors(t)
        end
    end
end

function print_test_results(ts::ReTestSet, fmt::Format;
                            depth::Int=0, bold::Bool=false, hasbroken::Bool=false,
                            maxidw::Int)
    # Calculate the overall number for each type so each of
    # the test result types are aligned
    upd = false

    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = get_test_counts(ts)
    total_pass   = passes + c_passes
    total_fail   = fails  + c_fails
    total_error  = errors + c_errors
    total_broken = broken + c_broken
    total = total_pass + total_fail + total_error + total_broken

    # if no tests, count that as a pass:
    dig_pass   = total_pass   > 0 || total == 0 ? ndigits(total_pass)   : 0
    dig_fail   = total_fail   > 0               ? ndigits(total_fail)   : 0
    dig_error  = total_error  > 0               ? ndigits(total_error)  : 0
    dig_broken = total_broken > 0 || hasbroken  ? ndigits(total_broken) : 0

    # max(1, ...) : we always print something at least in "Pass", even when no tests
    nprinted = max(1, (total_pass > 0) + (total_fail > 0) +
                      (total_error > 0) + (total_broken > 0))
    if nprinted == 1 && total_pass > 0 && !hasbroken
        # do not print "Total" when only "Pass" column is printed
        total = 0
    end
    dig_total = total > 0 || hasbroken ? ndigits(total) : 0

    # For each category, take max of digits and header width if there are
    # tests of that type
    fail_width   = dig_fail   > 0 ? max(6,   dig_fail) : 0
    error_width  = dig_error  > 0 ? max(6,  dig_error) : 0
    broken_width = dig_broken > 0 ? max(6, dig_broken) : 0
    total_width  = dig_total  > 0 ? max(6,  dig_total) : 0
    # heuristic: if we print total because of a broken test, still print "Pass",
    # as it's likely that later on a passing test will be printed
    pass_width   = dig_pass   > 0 || total_width > 0 ? max(6,   dig_pass) : 0

    if pass_width > fmt.pass_width
        upd = true
        fmt.pass_width = pass_width
    else
        pass_width = fmt.pass_width
    end
    if fail_width > fmt.fail_width
        upd = true
        fmt.fail_width = fail_width
    else
        fail_width = fmt.fail_width
    end
    if error_width > fmt.error_width
        upd = true
        fmt.error_width = error_width
    else
        error_width = fmt.error_width
    end
    if broken_width > fmt.broken_width
        upd = true
        fmt.broken_width = broken_width
    else
        broken_width = fmt.broken_width
    end
    if total_width > fmt.total_width
        upd = true
        fmt.total_width = total_width
    else
        total_width = fmt.total_width
    end

    # Calculate the alignment of the test result counts by
    # recursively walking the tree of test sets
   if !ts.overall # otherwise, we don't print recursively
       align = get_alignment(ts, 0)
   end

    if !ts.overall && align > fmt.desc_align
        upd = true
        fmt.desc_align = align
    else
        align = fmt.desc_align
    end

    # Print the outer test set header once
    if upd
        if maxidw > 0
            print(' '^(maxidw + 2)) # +2 for "| " after number
        end
        printstyled(rpad("", align, " "), "  ", " "; bold=true, color=:white)
        if pass_width > 0
            printstyled(lpad("Pass", pass_width, " "), "  "; bold=true, color=:green)
        end
        if fail_width > 0
            printstyled(lpad("Fail", fail_width, " "), "  "; bold=true, color=Base.error_color())
        end
        if error_width > 0
            printstyled(lpad("Error", error_width, " "), "  "; bold=true, color=Base.error_color())
        end
        if broken_width > 0
            printstyled(lpad("Broken", broken_width, " "), "  "; bold=true, color=Base.warn_color())
        end
        if total_width > 0
            printstyled(lpad("Total", total_width, " "), fmt.stats ? "  " : ""; bold=true, color=Base.info_color())
        end
        if fmt.stats
            # copied from Julia/test/runtests.jl
            compile_header = VERSION >= v"1.6-" ? " Compile /" : ""
            printstyled("|  Time /$compile_header GC |   Alloc   ΔRSS |", color=:white)
        end
        println()
    end

    # Recursively print a summary at every level
    print_counts(ts, fmt, depth, align,
                 pass_width, fail_width, error_width, broken_width, total_width;
                 bold=bold, maxidw=maxidw)
end

# Called at the end of a @testset, behaviour depends on whether
# this is a child of another testset, or the "root" testset
function finish(ts::ReTestSet, chan)
    # If we are a nested test set, do not print a full summary
    # now - let the parent test set do the printing
    if get_testset_depth() != 0
        # Attach this test set to the parent test set
        parent_ts = get_testset()
        record(parent_ts, ts)
        return ts
    end
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = get_test_counts(ts)
    total_pass   = passes + c_passes
    total_fail   = fails  + c_fails
    total_error  = errors + c_errors
    total_broken = broken + c_broken
    total = total_pass + total_fail + total_error + total_broken

    # Finally throw an error as we are the outermost test set
    if total != total_pass + total_broken
        # Get all the error/failures and bring them along for the ride
        efs = filter_errors(ts)
        ts.exception = TestSetException(total_pass, total_fail, total_error,
                                        total_broken, efs)
    end

    put!(chan.out, ts)
    if myid() == 1
        take!(chan.compute)
    end

    # return the testset so it is returned from the @testset macro
    ts
end

# Recursive function that finds the column that the result counts
# can begin at by taking into account the width of the descriptions
# and the amount of indentation. If a test set had no failures, and
# no failures in child test sets, there is no need to include those
# in calculating the alignment
function get_alignment(ts::ReTestSet, depth::Int)
    # The minimum width at this depth is
    ts_width = 2*depth + length(ts.description)
    # If all passing, no need to look at children
    !ts.anynonpass && return ts_width
    # Return the maximum of this width and the minimum width
    # for all children (if they exist)
    isempty(ts.results) && return ts_width
    child_widths = map(t->get_alignment(t, depth+1), ts.results)
    return max(ts_width, maximum(child_widths))
end
get_alignment(ts, depth::Int) = 0

# Recursive function that fetches backtraces for any and all errors
# or failures the testset and its children encountered
function filter_errors(ts::ReTestSet)
    efs = []
    for t in ts.results
        if isa(t, ReTestSet)
            append!(efs, filter_errors(t))
        elseif isa(t, Union{Fail, Error})
            append!(efs, [t])
        end
    end
    efs
end

# Recursive function that counts the number of test results of each
# type directly in the testset, and totals across the child testsets
function get_test_counts(ts::ReTestSet)
    passes, fails, errors, broken = ts.n_passed, 0, 0, 0
    c_passes, c_fails, c_errors, c_broken = 0, 0, 0, 0
    for t in ts.results
        isa(t, Fail)   && (fails  += 1)
        isa(t, Error)  && (errors += 1)
        isa(t, Broken) && (broken += 1)
        if isa(t, ReTestSet)
            np, nf, ne, nb, ncp, ncf, nce , ncb = get_test_counts(t)
            c_passes += np + ncp
            c_fails  += nf + ncf
            c_errors += ne + nce
            c_broken += nb + ncb
        end
    end
    ts.anynonpass = (fails + errors + c_fails + c_errors > 0)
    return passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken
end

anyfailed(ts::ReTestSet) = any(t -> t isa Union{Fail,Error}, ts.results)

# Recursive function that prints out the results at each level of
# the tree of test sets
function print_counts(ts::ReTestSet, fmt::Format, depth, align,
                      pass_width, fail_width, error_width, broken_width, total_width;
                      bold, maxidw)
    # Count results by each type at this level, and recursively
    # through any child test sets
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = get_test_counts(ts)
    subtotal = passes + fails + errors + broken + c_passes + c_fails + c_errors + c_broken
    # Print test set header, with an alignment that ensures all
    # the test results appear above each other

    style = bold ? (bold=bold, color=:white) : NamedTuple()
    if maxidw > 0
        if ts.id != 0
            printstyled(lpad(ts.id, maxidw), "| ", color = :light_black, bold=true)
        else
            print(' '^(maxidw+2))
        end
    end

    printstyled(rpad(string("  "^depth, ts.description), align, " "); style...)

    np = passes + c_passes
    nf = fails + c_fails
    ne = errors + c_errors
    nb = broken + c_broken

    print_total = true

    if ts.overall && endswith(ts.description, ':')
        # header, we don't print the '|' nor anything afterwards
        @assert np == 0 && nf == 0 && ne == 0 && nb == 0
        print_total = false
    elseif np > 0 || np == 0 && nf == 0 && ne == 0 && nb == 0
        # print `0` in warn color instead of "No tests" like in Test module,
        # which messes up alignments (and am too lazy to fix)
        printstyled(" | ", bold=false)
        printstyled(lpad(string(np), pass_width, " "), "  ",
                    color = np > 0 ? :green : Base.warn_color(), bold=bold)
    elseif pass_width > 0 # TODO: isn't this condition always true here?
        # No passes at this level, but some at another level
        printstyled(" | ", bold=false)
        print(lpad(" ", pass_width), "  ")
    end

    if nf > 0
        printstyled(lpad(string(nf), fail_width, " "), "  ", color=Base.error_color(), bold=bold)
    elseif fail_width > 0
        # No fails at this level, but some at another level
        print(lpad(" ", fail_width), "  ")
    end

    if ne > 0
        printstyled(lpad(string(ne), error_width, " "), "  ", color=Base.error_color(), bold=bold)
    elseif error_width > 0
        # No errors at this level, but some at another level
        print(lpad(" ", error_width), "  ")
    end

    if nb > 0
        printstyled(lpad(string(nb), broken_width, " "), "  ", color=Base.warn_color(), bold=bold)
    elseif broken_width > 0
        # None broken at this level, but some at another level
        print(lpad(" ", broken_width), "  ")
    end

    if total_width > 0 && print_total
        printstyled(lpad(string(subtotal), total_width, " "), fmt.stats ? "  " : "", color=Base.info_color(), bold=bold)
    end

    if fmt.stats && print_total # copied from Julia/test/runtests.jl
        ts.overall && set_timed!(ts)
        timed = ts.timed

        # we don't want to report zeros, which makes it hard to spot non-zeros
        function hide_zero(str, unit)
            strip(str, ' ') in ("0.0", "0.00") ?
                ' '^(1+length(str)) : # +1 for unit
                str * unit
        end

        time_str = hide_zero(@sprintf("%6.2f", timed.time), "s")
        printstyled("| ", time_str, " ", color=:white)
        if VERSION >= v"1.6-"
            compile_str = all(==(' '), time_str) ?
                ' '^6 : # print percentages only if time itself is shown!
                        # (also, this can result in weird things, like "30663.3%",
                        # if e.g. there was no @test in the @testset)
                hide_zero(@sprintf("%5.1f", timed.compile_time / 10^7 / timed.time), "%")
            # can be >= 100% !?
            printstyled(compile_str, " ", color=:white)
        end
        gc_str = all(==(' '), time_str) ?
            ' '^5 :
            hide_zero(@sprintf("%4.1f", 100 * timed.gctime / timed.time), "%")
        printstyled(gc_str, " | ", color=:white)

        alloc_str = hide_zero(@sprintf("%6.1f", timed.bytes / 2^20), "M")
        printstyled(alloc_str, " ", color=:white)

        rss_str = hide_zero(@sprintf("%5.1f", timed.rss / 2^20), "M")
        printstyled(rss_str, " |", color=:white)
    end
    println()

    # Only print results at lower levels if we had failures or if the user
    # wants.
    if !ts.overall && ((np + nb != subtotal) || (ts.verbose))
        for t in ts.results
            if isa(t, ReTestSet)
                print_counts(t, fmt, depth + 1, align,
                             pass_width, fail_width, error_width, broken_width, total_width,
                             bold=false, maxidw=maxidw)
            end
        end
    end
end

#-----------------------------------------------------------------------

default_rng() = isdefined(Random, :default_rng) ?
    Random.default_rng() :
    Random.GLOBAL_RNG

function make_retestset(mod, desc, id, verbose, remove_last=false)
    _testsets = get(task_local_storage(), :__BASETESTNEXT__, Test.AbstractTestSet[])
    @assert !(remove_last && isempty(_testsets))
    testsets = @view _testsets[1:end-remove_last]
    ReTestSet(mod, desc, id; verbose=verbose,
              parent = isempty(testsets) ? nothing : testsets[end])
end

# HACK: we re-use the same macro name `@testset` for actual execution (like in `Test`)
# as for the one documented in ReTest (deferred execution), because otherwise
# packages wouldn't know about the former, say named @retestset (which has to be evaluated
# within packages' modules). We could export @retestset but this is fragile;
# is there a simple alternative?
# In the meantime, we create this semi-catch-all method, called from make_ts,
# so that when arguments are messed up (forgot to update call in make_ts), we don't end-up
# calling the real-catch-all method from InlineTest (which is again deferred), which
# regularly leads to confusing behavior

# try to not mess up first three arguments!
macro testset(mod::Module, isfinal::Bool, pat::Pattern, x...)
    error("invalid arguments")
end

# non-inline testset with regex filtering support
macro testset(mod::Module, isfinal::Bool, pat::Pattern, id::Int64, desc::Union{String,Expr},
              options, pastresults::Dict, stats::Bool, chan, body)
    Testset.testset_beginend(mod, isfinal, pat, id, desc,
                             options, pastresults, stats, chan, body, __source__)
end

macro testset(mod::Module, isfinal::Bool, pat::Pattern, id::Int64, desc::Union{String,Expr},
              options, pastresults::Dict, stats::Bool, chan, loops, body)
    Testset.testset_forloop(mod, isfinal, pat, id, desc,
                            options, pastresults, stats, chan, loops, body, __source__)
end

"""
Generate the code for a `@testset` with a `begin`/`end` argument
"""
function testset_beginend(mod::Module, isfinal::Bool, pat::Pattern, id::Int64, desc, options,
                          pastresults::Dict, stats::Bool, chan, tests, source)
    # Generate a block of code that initializes a new testset, adds
    # it to the task local storage, evaluates the test(s), before
    # finally removing the testset and giving it a chance to take
    # action (such as reporting the results)
    desc = esc(desc)
    ex = quote
        local ts = make_retestset($mod, $desc, $id, $(options.transient_verbose))

        if !$isfinal || matches($pat, ts.subject, ts)
            local ret
            if nworkers() == 1 && get_testset_depth() == 0 && $(chan.preview) !== nothing
                put!($(chan.preview), $desc)
            end
            push_testset(ts)
            # we reproduce the logic of guardseed, but this function
            # cannot be used as it changes slightly the semantic of @testset,
            # by wrapping the body in a function
            local RNG = default_rng()
            local oldrng = copy(RNG)
            try
                # RNG is re-seeded with its own seed to ease reproduce a failed test
                if VERSION >= v"1.7.0-DEV.1225"
                    Random.seed!(Random.GLOBAL_SEED)
                else
                    Random.seed!(RNG.seed)
                end
                let
                    ts.timed = @stats $stats $(esc(tests))
                end
            catch err
                err isa InterruptException && rethrow()
                # something in the test block threw an error. Count that as an
                # error in this test set
                record(ts, Error(:nontest_error, Expr(:tuple), err,
                                 Base.catch_stack(), $(QuoteNode(source))))
            finally
                copy!(RNG, oldrng)
                $pastresults[ts.subject] = !anyfailed(ts)
                pop_testset()
                ret = finish(ts, $chan)
            end
            ret
        end
    end
    # preserve outer location if possible
    if tests isa Expr && tests.head === :block &&
        !isempty(tests.args) && tests.args[1] isa LineNumberNode

        ex = Expr(:block, tests.args[1], ex)
    end
    return ex
end

"""
Generate the code for a `@testset` with a `for` loop argument
"""
function testset_forloop(mod::Module, isfinal::Bool, pat::Pattern, id::Int64,
                         desc::Union{String,Expr}, options, pastresults::Dict, stats, chan,
                         loops, tests, source)

    desc = esc(desc)
    blk = quote
        local ts0 = make_retestset($mod, $desc, $id, $(options.transient_verbose),
                                   !first_iteration)

        if !$isfinal || matches($pat, ts0.subject, ts0)
            # Trick to handle `break` and `continue` in the test code before
            # they can be handled properly by `finally` lowering.
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts, $chan))
                # it's 1000 times faster to copy from tmprng rather than calling Random.seed!
                copy!(RNG, tmprng)
            end
            ts = ts0
            if nworkers() == 1 && get_testset_depth() == 0 && $(chan.preview) !== nothing
                put!($(chan.preview), ts.description)
            end
            push_testset(ts)
            first_iteration = false
            try
                let
                    ts.timed = @stats $stats $(esc(tests))
                end
                $pastresults[ts.subject] = !anyfailed(ts)
            catch err
                err isa InterruptException && rethrow()
                # Something in the test block threw an error. Count that as an
                # error in this test set
                record(ts, Error(:nontest_error, Expr(:tuple), err, Base.catch_stack(), $(QuoteNode(source))))
                $pastresults[ts.subject] = false
            end
        end
    end
    quote
        local arr = Vector{Any}()
        local first_iteration = true
        local ts
        local RNG = default_rng()
        local oldrng = copy(RNG)
        if VERSION >= v"1.7.0-DEV.1225"
            Random.seed!(Random.GLOBAL_SEED)
        else
            Random.seed!(RNG.seed)
        end
        local tmprng = copy(RNG)
        try
            let
                $(Expr(:for, Expr(:block, [esc(v) for v in loops]...), blk))
            end
        finally
            # Handle `return` in test body
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts, $chan))
            end
            copy!(RNG, oldrng)
        end
        arr
    end
end

function set_timed!(ts)
    if ts.overall
        foreach(get_timed!, ts.results)
    end
    ts.timed = NamedTuple{(:time, :bytes, :gctime, :rss, :compile_time)}(
        ntuple(Val(5)) do i
            # init=0.0 kwarg not available on old Julia
            Float64(sum(tsc.timed[i] for tsc in ts.results))
        end)
end

get_timed!(ts) = isempty(ts.timed) ? set_timed!(ts) : ts

# adapted from @timed in Julia/base/timing.jl
# also, @timed inserts a `while false; end` compiler heuristic, which destroys perfs here
macro stats(yes, ex)
    quote
        if $yes
            local stats = Base.gc_num()
            local elapsedtime = time_ns()
            local rss = Sys.maxrss()
            local compile_time = cumulative_compile_time_ns()
        end
        local val = $(esc(ex))
        if $yes
            elapsedtime = time_ns() - elapsedtime
            local diff = Base.GC_Diff(Base.gc_num(), stats)
            rss = Sys.maxrss() - rss
            compile_time = cumulative_compile_time_ns() - compile_time
            # COMPAT: on Julia 1.1, the form `(time=..., bytes=..., ...)` doesn't work
            # (macro name mangling with `#`, e.g. (#115#time = ..., ))
            NamedTuple{(:time, :bytes, :gctime, :rss, :compile_time)}(
                (elapsedtime/1e9, diff.allocd, diff.total_time/1e9, rss, compile_time))
        else
            NamedTuple()
        end
    end
end

cumulative_compile_time_ns() =
    isdefined(Base, :cumulative_compile_time_ns) ?
        Base.cumulative_compile_time_ns() :
        UInt(0)

end # module
