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


const INLINE_TEST = Ref{Symbol}(:__INLINE_TEST__)
const TESTED_MODULES = Module[]

include("testset.jl")

using .Testset: Testset, @testsetr

__init__() = INLINE_TEST[] = gensym()


struct TestsetExpr
    desc::Union{String,Expr}
    loops::Union{Expr,Nothing}
    body::Expr
    final::Bool
end

function tests(m::Module)
    inline_test::Symbol = m ∈ (ReTest, ReTest.ReTestTest) ? :__INLINE_TEST__ : INLINE_TEST[]
    if !isdefined(m, inline_test)
        @eval m $inline_test = []
        push!(TESTED_MODULES, m)
    end
    getfield(m, inline_test)
end

replacetestset(x) = (x, missing, false)

# replace unqualified `@testset` by @testsetr
# return also (as 3nd element) whether the expression contains a (possibly nested) @testset
# (if a testset is "final", i.e. without nested testsets, then the Regex matching logic
# is different: we then don't use "partial matching")
# the 2nd returned element is whether the testset is final (used only in `addtest`)
function replacetestset(x::Expr)
    if x.head === :macrocall && x.args[1] === Symbol("@testset")
        body  = map(replacetestset, x.args[2:end])
        final = !any(last, body)
        (Expr(:let,
              :($(Testset.FINAL[]) = $final),
              Expr(:macrocall, Expr(:., :ReTest, QuoteNode(Symbol("@testsetr"))),
                   map(first, body)...)),
         final,
         true)
    else
        body = map(replacetestset, x.args)
        (Expr(x.head, map(first, body)...),
         missing, # missing: a non-testset doesn't have a "final" attribute...
         any(last, body))
    end
end

function addtest(args::Tuple, m::Module)
    length(args) == 2 || error("unsupported @testset")

    desc = args[1]
    desc isa String || Meta.isexpr(desc, :string) || error("unsupported @testset")

    body = args[2]
    isa(body, Expr) || error("Expected begin/end block or for loop as argument to @testset")
    if body.head === :for
        isloop = true
    elseif body.head === :block
        isloop = false
    else
        error("Expected begin/end block or for loop as argument to @testset")
    end

    if isloop
        loops = body.args[1]
        expr, _, has_testset = replacetestset(body.args[2])
        final = !has_testset
        push!(tests(m), TestsetExpr(desc, loops, expr, final))
    else
        ts, final, _ = replacetestset(:(@testset $desc $body))
        push!(tests(m), TestsetExpr(desc, nothing, ts, final))
    end
    nothing
end

"""
    @testset args...

Similar to `Test.@testset args...`, but the contained tests are not run immediately,
and are instead stored for later execution, triggered by `runtests()`.
Invocations of `@testset` can be nested, but qualified invocations of
`ReTest.@testset` can't.
Internally, `@testset` invocations are converted to `Test.@testset` at execution time.
"""
macro testset(args...)
    Expr(:call, :addtest, args, __module__)
end

"""
    runtests([m::Module], pattern = r""; [wrap::Bool])

Run all the tests declared in `@testset` blocks, within `m` if specified,
or within all currently loaded modules otherwise.
The `wrap` keyword specifies whether the collection of `@testset` expressions
should be grouped according to the parent modules within a top-level `@testset`.
The default is `wrap=false` when `m` is specified, `true` otherwise.

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
A testset is guaranteed to run only when its parent's subject "partially matches"
`pattern::Regex` (in PCRE2 parlance, i.e. `subject` might be the prefix of a string
matched by `pattern`) and its subject matches `pattern`.

If the passed `pattern` is a string, then it is wrapped in a `Regex` prefixed with
`".*"`, and must match literally the subjects.
This means for example that `"a|b"` will match a subject like `"a|b"` but not like `"a"`
(only in Julia versions >= 1.3; in older versions, the regex is simply created as
`Regex(".*" * pattern)`).
The `".*"` prefix is intended to allow matching subjects of nested testsets,
e.g. in the example above, `r".*b"` partially matches the subject `"/a"` and
matches the subject `"/a/b"` (so the corresponding nested testset is run),
whereas `r"b"` does not partially match `"/a"`, even if it matches `"/a/b"`,
so the testset is not run.

Note: this function executes each (top-level) `@testset` block using `eval` *within* the
module in which it was written (e.g. `m`, when specified).
"""
function runtests(m::Module, pattern::Union{AbstractString,Regex} = r""; wrap::Bool=false)
    regex = pattern isa Regex ?
        pattern :
        if VERSION >= v"1.3"
            r".*" * pattern
        else
            Regex(".*" * pattern)
        end

    partial = partialize(regex)
    matches(desc, final) = Testset.partialoccursin((partial, regex)[1+final],
                                                   string('/', desc, final ? "" : "/"))

    if wrap
        Core.eval(m, :(ReTest.Test.@testset $("Tests for module $m") begin
                           $(map(ts -> wrap_ts(partial, regex, ts), tests(m))...)
                       end))
    else
        for ts in tests(m)
            # it's faster to evel in a loop than to eval a block containing tests(m)
            desc = ts.desc
            if ts.loops === nothing # begin/end testset
                @assert desc isa String
                # bypass evaluation if we know statically that testset won't be run
                matches(desc, ts.final) ||
                    continue
                Core.eval(m, wrap_ts(partial, regex, ts))
            else # for-loop testset
                loops = ts.loops
                xs = Core.eval(m, loops.args[2]) # loop values
                # we eval the description to a string for each values, and if at least one matches
                # the filtering-regex, then we must instantiate the testset (otherwise, it's skipped)
                skip = true
                for x in xs
                    # TODO: do not assign loop.args[1] in m ?
                    Core.eval(m, Expr(:(=), loops.args[1], x))
                    descx = Core.eval(m, desc)
                    if matches(descx, ts.final)
                        skip = false
                        break
                    end
                end
                skip && continue
                Core.eval(m, wrap_ts(partial, regex, ts, xs))
            end
        end
    end
    nothing
end

function wrap_ts(partial, regex, ts::TestsetExpr, loopvals=nothing)
    if ts.loops === nothing
        quote
            let $(Testset.REGEX[]) = ($partial, $regex)
                $(ts.body)
            end
        end
    else
        loopvals = something(loopvals, ts.loops.args[2])
        quote
            let $(Testset.REGEX[]) = ($partial, $regex),
                $(Testset.FINAL[]) = $(ts.final)

                ReTest.@testsetr $(ts.desc) for $(ts.loops.args[1]) in $loopvals
                    $(ts.body)
                end
            end
        end
    end
end

function runtests(pattern::Union{AbstractString,Regex} = r""; wrap::Bool=true)
    # TESTED_MODULES is not up-to-date w.r.t. package modules which have
    # precompilation, so we have to also look in Base.loaded_modules
    # TODO: look recursively in "loaded modules" which use ReTest for sub-modules
    for m in unique(Iterators.flatten((values(Base.loaded_modules), TESTED_MODULES)))
        if isdefined(m, INLINE_TEST[])
            # will automatically skip ReTest and ReTest.ReTestTest
            runtests(m, pattern, wrap=wrap)
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
    runtests(ReTestTest, wrap=true)
end

end # module ReTest
