# like occursin, but handles partial matches
function partialoccursin(r::Regex, s::AbstractString; offset::Integer=0)
    Base.compile(r)
    e = exec_r(r.regex, String(s), offset, r.match_options)
    e == -2 || e >= 0
end

if isdefined(Base.PCRE, :get_local_match_context)
    get_local_match_context() = Base.PCRE.get_local_match_context()
else
    get_local_match_context() = Base.PCRE.MATCH_CONTEXT[]
end

# from base/pcre.jl
# we only changed the return value, to allow detecting partial matches
function exec(re, subject, offset, options, match_data)
    if !(subject isa Union{String,SubString{String}})
        subject = String(subject)
    end
    rc = ccall((:pcre2_match_8, Base.PCRE.PCRE_LIB), Cint,
               (Ptr{Cvoid}, Ptr{UInt8}, Csize_t, Csize_t, UInt32, Ptr{Cvoid}, Ptr{Cvoid}),
               re, subject, ncodeunits(subject), offset, options, match_data, get_local_match_context())
    # rc == -1 means no match, -2 means partial match.
    rc < -2 && error("PCRE.exec error: $(err_message(rc))")
    return rc
end

function exec_r(re, subject, offset, options)
    match_data = Base.PCRE.create_match_data(re)
    ans = exec(re, subject, offset, options, match_data)
    Base.PCRE.free_match_data(match_data)
    return ans
end
