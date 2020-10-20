module InlineTest

export runtests, @testset, @test, @test_throws, Test

using Test: Test, @test, @test_throws

const INLINE_TEST = Ref{Symbol}(:__INLINE_TEST__)

include("testset.jl")

using .Testset: Testset, @testsetr

__init__() = INLINE_TEST[] = gensym()


function tests(m)
    inline_test::Symbol = m ∈ (InlineTest, InlineTest.InlineTestTest) ? :__INLINE_TEST__ : INLINE_TEST[]
    if !isdefined(m, inline_test)
        @eval m $inline_test = Expr[]
    end
    getfield(m, inline_test)
end

replacetestset(x) = x, false

# replace unqualified `@testset` by @testsetr
# return also (as 2nd element) whether the expression contains a (possibly nested) @testset
# (if a testset is "final", i.e. without nested testsets, then the Regex matching logic
# is different: we then don't use "partial matching")
function replacetestset(x::Expr)
    if x.head === :macrocall && x.args[1] === Symbol("@testset")
        body  = map(replacetestset, x.args[2:end])
        final = !any(last, body)
        (Expr(:let,
              :($(Testset.FINAL[]) = $final),
              Expr(:macrocall, Expr(:., :InlineTest, QuoteNode(Symbol("@testsetr"))),
                   map(first, body)...)),
         true)
    else
        body = map(replacetestset, x.args)
        (Expr(x.head, map(first, body)...),
         any(last, body))
    end
end

function addtest(args::Tuple, m::Module)
    ts, _ = replacetestset(:(@testset($(args...))))
    push!(tests(m), ts)
    nothing
end

"""
    @testset args...

Similar to `Test.@testset args...`, but the contained tests are not run immediately,
and are instead stored for later execution, triggered by `runtests()`.
Invocations of `@testset` can be nested, but qualified invocations of
`InlineTest.@testset` can't.
Internally, `@testset` invocations are converted to `Test.@testset` at execution time.
"""
macro testset(args...)
    Expr(:call, :addtest, args, __module__)
end

"""
    runtests([m::Module]; [wrap::Bool])

Run all the tests declared in `@testset` blocks, within `m` if specified,
or within all currently loaded modules otherwise.
The `wrap` keyword specifies whether the collection of `@testset` blocks derived
from `@testset` declarations should be grouped within a top-level `@testset`.
The default is `wrap=false` when `m` is specified, `true` otherwise.

Note: this function executes each (top-level) `@testset` block using `eval` *within* the module
in which it was written (e.g. `m`, when specified).
"""
function runtests(m::Module, regex::Regex = r""; wrap::Bool=false)
    partial = partialize(regex)
    ex = Expr(:let,
              Expr(:(=), Testset.REGEX[], (partial, regex)),
              Expr(:block, tests(m)...))

    if wrap
        ex = :(InlineTest.Test.@testset $("Tests for module $m") begin
               $ex
               end)
    end
    Core.eval(m, ex)
    nothing
end

function runtests(; wrap::Bool=true)
    foreach(values(Base.loaded_modules)) do m
        if isdefined(m, INLINE_TEST[]) # will automatically skip InlineTest and InlineTest.InlineTestTest
            ts = runtests(m, wrap=wrap)
        end
    end
end

function partialize(r::Regex)
    if r.match_options & (Base.PCRE.PARTIAL_HARD | Base.PCRE.PARTIAL_SOFT) == 0
        Regex(r.pattern, r.compile_options, r.match_options | Base.PCRE.PARTIAL_SOFT)
    else
        r
    end
end

module InlineTestTest

using ..InlineTest
@testset "test Test in sub-module" begin
    @test 1 == 1
end

end # module InlineTestTest

@testset "self test" begin
    @assert typeof(@__MODULE__) == Module
    @test 1 != 2
    runtests(InlineTestTest)
    runtests(InlineTestTest, wrap=true)
end

end # module InlineTest
