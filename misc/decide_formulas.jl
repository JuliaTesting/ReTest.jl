# Find a "formula" for the `decide` function in `resolve!`
# Given the result of `matches` (`b` below) and the value of `static` (`a` below),
# the goal is to find the simplest formula involving few basic functions
# with the same "truth table" as `r` below, using three-valued logic

r(a, b) = # a: static, b: match
    if a === true
        coalesce(b, false)
    elseif a === false
        b === missing
    else
        coalesce(b, true)
    end

# give a name to some basic functions:
arg1(a, b) = a
arg2(a, b) = b
narg1(a, b) = !a
narg2(a, b) = !b
egal(a, b) = a === b # have to give a name, because of a Julia bug:
                     # https://github.com/JuliaLang/julia/issues/40612
negal(a, b) = a !== b
impl(a, b) = !a | b
equiv(a, b) = impl(a, b) & impl(b, a)
revcoalesce(a, b) = coalesce(b, a)

# composing directly these functions lead to too much compilation for brute force,
# so we use truth tables instead

struct TT
    t::Matrix{Int}
    n::Symbol # name
end

TT(n::Symbol = :?) = TT(zeros(Bool, 3, 3), n)

# init a truthtable from a binary function
function init!(tt::TT, f::Function)
    m = (true, false, missing)
    r(x) = x === missing ? 3 : x ? 1 : 2
    for a = 1:3, b = 1:3
        tt.t[a, b] = r(f(m[a], m[b]))
    end
    tt
end

# compose truthtables: tt(a, b) = o(i1(a, b), i2(a, b))
function init!(tt::TT, o::TT, i1::TT, i2::TT)
    for a=1:3, b=1:3
        tt.t[a, b] = o.t[i1.t[a, b], i2.t[a, b]]
    end
    tt
end

init!(tt::TT, src::TT) = (copy!(tt.t, src.t); tt)

TT(f::Function) = init!(TT(nameof(f)), f)
TT(o::TT, i1::TT, i2::TT) = init!(TT(), o, i1, i2)

Base.show(io::IO, tt::TT) = print(io, tt.n)

const R = TT(r)

# compute distance from R
function check(tt::TT)
    # TODO: add multi-args `count` to `Base` ?
    dst = 0
    for i in 1:9
        dst += tt.t[i] != R.t[i]
    end
    dst
end

function solve(simple=false)
    fns = TT.([arg1, arg2, narg1, narg2, coalesce, |, &, ==, !=,  egal, negal])
    !simple && push!(fns, TT(revcoalesce), TT(impl), TT(equiv))

    F = TT(); O2 = TT()

    c = 0
    iter = 0
    for fn in fns, o1 in fns, o2 in fns, i1 in fns, i2 in fns
        iter += 1
        init!(O2, o2, i1, i2)
        init!(F, fn, o1, O2)

        if check(F) == 0
            if simple
                issimple = false
                for arg in (:arg1, :arg2)
                    issimple |= any([fn, o1, i1, i2, o2]) do f
                        f.n == arg
                    end
                end
                issimple || continue
            end
            c += 1
            @show fn, o1, o2, i1, i2
        end
    end
    (niter = iter, nsolutions = c)
end

sol1(s, m) = s | m === coalesce(s == m, m)
sol5(s, m) = s | m === (coalesce(s, m) == m)
sol2(s, m) = m === coalesce(s, m) | !m
sol3(s, m) = m === coalesce(s, m) | (s == m)
sol4(s, m) = m === (coalesce(s, m) == s | m)

for sol in (sol1, sol2, sol3, sol4, sol5)
    t = TT(sol)
    @assert check(t) == 0
end
