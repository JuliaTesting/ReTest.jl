module ReTest

export runtests, @testset

# from Test:
export Test,
    @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated,
    @inferred,
    detect_ambiguities, detect_unbound_args

using Test: Test,
    @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated,
    @inferred,
    detect_ambiguities, detect_unbound_args

using InlineTest: @testset, InlineTest, get_tests, TESTED_MODULES, INLINE_TEST

include("testset.jl")

using .Testset: Testset, Format


mutable struct TestsetExpr
    desc::Union{String,Expr}
    loops::Union{Expr,Nothing}
    parent::Union{TestsetExpr,Nothing}
    children::Vector{TestsetExpr}
    strings::Vector{String}
    loopvalues::Any
    run::Bool
    body::Expr

    TestsetExpr(desc, loops, parent, children=TestsetExpr[]) =
        new(desc, loops, parent, children, String[])
end

isfor(ts::TestsetExpr) = ts.loops !== nothing
isfinal(ts::TestsetExpr) = isempty(ts.children)

# replace unqualified `@testset` by TestsetExpr
function replace_ts(x::Expr, parent)
    if x.head === :macrocall && x.args[1] === Symbol("@testset")
        @assert x.args[2] isa LineNumberNode
        ts = parse_ts(Tuple(x.args[3:end]), parent)
        parent !== nothing && push!(parent.children, ts)
        ts
    else
        body = map(z -> replace_ts(z, parent), x.args)
        Expr(x.head, body...)
    end
end

replace_ts(x, _) = x

# create a TestsetExpr from @testset's args
function parse_ts(args::Tuple, parent=nothing)
    length(args) == 2 || error("unsupported @testset")

    desc = args[1]
    desc isa String || Meta.isexpr(desc, :string) || error("unsupported @testset")

    body = args[2]
    isa(body, Expr) || error("Expected begin/end block or for loop as argument to @testset")
    if body.head === :for
        loops = body.args[1]
        tsbody = body.args[2]
    elseif body.head === :block
        loops = nothing
        tsbody = body
    else
        error("Expected begin/end block or for loop as argument to @testset")
    end

    ts = TestsetExpr(desc, loops, parent)
    ts.body = replace_ts(tsbody, ts)
    ts
end

function resolve!(mod::Module, ts::TestsetExpr, rx::Regex, force::Bool=false)
    strings = empty!(ts.strings)
    desc = ts.desc
    ts.run = force || isempty(rx.pattern)
    ts.loopvalues = nothing # unnecessary ?

    parentstrs = ts.parent === nothing ? [""] : ts.parent.strings

    if desc isa String
        for str in parentstrs
            ts.run && break
            new = str * '/' * desc
            if occursin(rx, new)
                ts.run = true
            else
                push!(strings, new)
            end
        end
    else
        loops = ts.loops
        @assert loops !== nothing
        xs = ()
        try
            xs = Core.eval(mod, loops.args[2])
            if !(xs isa Union{Array,Tuple}) # being conservative on target type
                # this catches e.g. the case where xs is a generator, then collect
                # fails because of a world-age problem (the function in xs is too "new")
                xs = collect(xs)
            end
            ts.loopvalues = xs
        catch
            xs = () # xs might have been assigned before the collect call
            if !ts.run
                @warn "could not evaluate testset-for iterator, default to inclusion"
            end
            ts.run = true
        end
        for x in xs # empty loop if eval above threw
            ts.run && break
            Core.eval(mod, Expr(:(=), loops.args[1], x))
            descx = Core.eval(mod, desc)::String
            for str in parentstrs
                new = str * '/' * descx
                if occursin(rx, new)
                    ts.run = true
                    break
                else
                    push!(strings, new)
                end
            end
        end
    end
    run = ts.run
    for tsc in ts.children
        run |= resolve!(mod, tsc, rx, ts.run)
    end
    ts.run = run
end

# convert a TestsetExpr into an actually runnable testset
function make_ts(ts::TestsetExpr, rx::Regex, format::Format)
    ts.run || return nothing

    if isfinal(ts)
        body = ts.body
    else
        body = make_ts(ts.body, rx, format)
    end
    if ts.loops === nothing
        quote
            @testset $(isfinal(ts)) $rx $(ts.desc) $format $body
        end
    else
        loopvals = something(ts.loopvalues, ts.loops.args[2])
        quote
            @testset $(isfinal(ts)) $rx $(ts.desc) $format $(ts.loops.args[1]) $loopvals $body
        end
    end
end

make_ts(x, rx, _) = x
make_ts(ex::Expr, rx, format) = Expr(ex.head, map(x -> make_ts(x, rx, format), ex.args)...)

"""
    runtests([m::Module], pattern = r""; dry::Bool=false, stats::Bool=false)

Run all the tests declared in `@testset` blocks, within `m` if specified,
or within all currently loaded modules otherwise.
If `dry` is `true`, don't actually run the tests, just print the descriptions
of the testsets which would (presumably) run.
If `stats` is `true`, print some time/memory statistics for each testset.

It's possible to filter run testsets by specifying `pattern`: the "subject" of a
testset is the concatenation of the subject of its parent `@testset`, if any,
with `"/\$description"` where `description` is the testset's description.
For example:
```julia
@testset "a" begin # subject == "/a"
    @testset "b" begin # subject is "/a/b"
    end
    @testset "c\$i" for i=1:2 # subjects are "/a/c1" & "/a/c2"
    end
end
```
A testset is guaranteed to run only when its subject matches `pattern`.
Moreover if a testset is run, its enclosing testset, if any, also has to run
(although not necessarily exhaustively, i.e. other nested testsets
might be filtered out).

If the passed `pattern` is a string, then it is wrapped in a `Regex` and must
match literally the subjects.
This means for example that `"a|b"` will match a subject like `"a|b"` but not like `"a"`
(only in Julia versions >= 1.3; in older versions, the regex is simply created as
`Regex(pattern)`).

Note: this function executes each (top-level) `@testset` block using `eval` *within* the
module in which it was written (e.g. `m`, when specified).
"""
function runtests(mod::Module, pattern::Union{AbstractString,Regex} = r"";
                  dry::Bool=false,
                  stats::Bool=false)
    regex = pattern isa Regex ? pattern :
        if VERSION >= v"1.3"
            r"" * pattern
        else
            Regex(pattern)
        end

    tests = get_tests(mod)

    desc_align = 0
    for idx in eachindex(tests)
        ts = tests[idx]
        if !(ts isa TestsetExpr)
            ts = tests[idx] = parse_ts(ts)
        end
        run = resolve!(mod, ts, regex)
        run || continue
        desc_len = length(ts.desc isa String ? ts.desc : ts.desc.args[1])
        desc_align = max(desc_align, desc_len)
    end

    format = Format(stats, desc_align)

    for ts in tests
        ts.run || continue
        if dry
            dryrun(mod, ts, regex)
            continue
        end
        mts = make_ts(ts, regex, format)
        Core.eval(mod, mts)
    end
    nothing
end

function runtests(pattern::Union{AbstractString,Regex} = r"")
    # TESTED_MODULES is not up-to-date w.r.t. package modules which have
    # precompilation, so we have to also look in Base.loaded_modules
    # TODO: look recursively in "loaded modules" which use ReTest for sub-modules
    for m in unique(Iterators.flatten((values(Base.loaded_modules), TESTED_MODULES)))
        if isdefined(m, INLINE_TEST[])
            # will automatically skip ReTest and ReTest.ReTestTest
            runtests(m, pattern)
        end
    end
end

function dryrun(mod::Module, ts::TestsetExpr, rx::Regex, parentsubj="", align::Int=0)
    ts.run || return
    desc = ts.desc

    if desc isa String
        subject = parentsubj * '/' * desc
        if isfinal(ts)
            occursin(rx, subject) || return
        end
        println(' '^align, desc)
        for tsc in ts.children
            dryrun(mod, tsc, rx, subject, align + 2)
        end
    else
        loopvals = ts.loopvalues
        if loopvals === nothing
            println(' '^align, desc)
            @warn "could not evaluate testset-for iterator, default to inclusion"
            return
        end
        for x in loopvals
            Core.eval(mod, Expr(:(=), ts.loops.args[1], x))
            descx = Core.eval(mod, desc)::String
            # avoid repeating ourselves, transform this iteration into a "begin/end" testset
            beginend = TestsetExpr(descx, nothing, ts.parent, ts.children)
            beginend.run = true
            dryrun(mod, beginend, rx, parentsubj, align)
        end
    end
end

module ReTestTest

using ..ReTest
@testset "test Test in sub-module" begin
    @test 1 == 1
end

end # module ReTestTest

@testset "self test" begin
    @assert typeof(@__MODULE__) == Module
    @test 1 != 2
    runtests(ReTestTest)
end

end # module ReTest
