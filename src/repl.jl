# Parsing of the `retest>` REPL mode input, see ext/ReTestREPLExt.jl. These
# functions are here instead of in the extension to make testing easier.

const REPL_COMMANDS = ["dry-run", "help", "load", "run", "run-failed", "set", "unset"]

const REPL_ALIASES = Dict("r" => "run", "dr" => "dry-run", "rf" => "run-failed",
                          "?" => "help")

# set by the REPL extension, which is the only place where TestEnv is loaded
global repl_load_tests_hook::Union{Nothing,Function} = nothing

function repl_help()
    print("""
          Press `}` at the start of an empty `julia>` line to enter `retest>` mode, and
          backspace on an empty line to exit. Each line must start with a command:

            run (r) ...         run the testsets matching the given arguments, which are
                                passed to `retest`; without arguments, run all tests
            dry-run (dr) ...    list matching testsets without running them
            run-failed (rf) ... run testsets which failed in the previous run
            load                activate the test environment of the active project and
                                load its tests from test/<Package>Tests.jl (only once
                                per session)
            set [key value]     show the preferences, or set one persistently, e.g.
                                `set verbose 2` or `set spin false`
            unset key...        remove the given preferences, e.g. `unset spin`
            help (?)            show this help

          The arguments of `run` and friends are translated as follows:

            run foo bar         testsets matching both "foo" and "bar"
            run foo -slow       testsets matching "foo" but not "slow"
            run "two words"     quoted pattern
            run 3  -3           select/exclude the testset with ID 3
            run :label          testsets tagged with :label
            run _dv2            keyword shorthand (here dry=true, verbose=2)
            run verbose=2       keyword argument
            run tag=[:a,:b]     keyword argument, whose value is Julia code unless quoted
          """)
end

struct ReplToken
    text::String                # for a `key=value` token, the value
    quoted::Bool                # whether `text` was given as a quoted string
    key::Union{Nothing,String}  # the `key` of a `key=value` token
end

function repl_tokenize(line::AbstractString)
    tokens = ReplToken[]
    word = ""
    inword = false
    quoted = false
    inquote = false
    key = nothing

    for c in line
        if inquote
            if c == '"'
                inquote = false
            else
                word *= c
            end
        elseif c == '"'
            inquote = true
            inword = true
            quoted = true
        elseif isspace(c)
            if inword
                push!(tokens, ReplToken(word, quoted, key))
                word = ""
                inword = false
                quoted = false
                key = nothing
            end
        elseif c == '=' && isnothing(key) && !quoted && Base.isidentifier(word)
            # `key=value`: what follows is the value, and `quoted` from now on
            # records the quoting of the value alone
            key = word
            word = ""
        else
            word *= c
            inword = true
        end
    end

    if inquote
        throw(ArgumentError("unterminated string in retest input: $line"))
    end
    if inword
        push!(tokens, ReplToken(word, quoted, key))
    end

    tokens
end

# `load` and other means (e.g. `using MyPackageTests`) both register in TESTED_MODULES
function repl_tests_loaded()
    update_TESTED_MODULES!() # drops stale entries, as `retest` does

    # ReTest's precompilation workload registers test modules, which don't count
    any(TESTED_MODULES) do mod
        !isnothing(mod) && Base.moduleroot(mod) !== @__MODULE__
    end
end

function repl_load_tests()
    hook = repl_load_tests_hook
    if isnothing(hook)
        error("`load` requires ReTest's REPL extension to be loaded")
    end

    if !isnothing(preferences_project)
        # only a previous `load` sets it, so `using MyPackageTests` doesn't get in the way
        error("`load` can be used only once per session, as the active project is " *
              "then a test environment; restart Julia to load the tests of another " *
              "package")
    end

    hook()
end

# `run` and friends go through here, rather than silently running nothing
function repl_retest(args...; kwargs...)
    if !repl_tests_loaded()
        error("no tests are loaded: use the `load` command to activate the test " *
              "environment of the active project and load its tests")
    end

    retest(args...; kwargs...)
end

# Only commands are completed, which appear only as the first word
function repl_completions(before_cursor::AbstractString)
    if any(isspace, before_cursor)
        return String[], ""
    end

    filter(c -> startswith(c, before_cursor), REPL_COMMANDS), String(before_cursor)
end

# before `load`, the active project is whatever the session started in, which isn't
# where a package's test preferences belong
function check_preferences_project()
    if isnothing(preferences_project)
        error("no tests are loaded: use the `load` command first, so that the " *
              "preferences are read from and stored in the project of the tested " *
              "package")
    end
end

function repl_set_preferences!(; kwargs...)
    check_preferences_project()
    set_preferences!(; kwargs...)
end

function repl_show_preferences()
    check_preferences_project()
    for kw in PREFERENCES
        println(kw, " = ", preference(kw))
    end
end

# the value of a preference, as a literal
function repl_value(tok::ReplToken)
    text = tok.text
    if tok.quoted
        return text
    end

    if text == "true"
        true
    elseif text == "false"
        false
    elseif text == "nothing"
        nothing
    else
        int = tryparse(Int, text)
        if !isnothing(int)
            return int
        end

        float = tryparse(Float64, text) # also handles "inf"
        if !isnothing(float)
            return float
        end

        text
    end
end

# a preference name is a bare identifier, not a pattern or a `key=value`
function repl_prefname(tok::ReplToken)
    if tok.quoted || !isnothing(tok.key) || !Base.isidentifier(tok.text)
        throw(ArgumentError("invalid preference name: $(repr(tok.text))"))
    end

    check_preference_name(Symbol(tok.text))
end

function repl_set(tokens)
    if isempty(tokens)
        return Expr(:call, repl_show_preferences)
    end

    if length(tokens) != 2
        throw(ArgumentError("set expects a preference name and a value, " *
                            "e.g. `set verbose 2`"))
    end

    key, val = tokens

    Expr(:call, repl_set_preferences!,
         Expr(:parameters, Expr(:kw, repl_prefname(key), repl_value(val))))
end

function repl_unset(tokens)
    if isempty(tokens)
        throw(ArgumentError("unset expects one or more preference names, " *
                            "e.g. `unset spin`"))
    end

    # `missing` makes `set_preferences!` delete the preference
    prefs = map(tok -> Expr(:kw, repl_prefname(tok), missing), tokens)

    Expr(:call, repl_set_preferences!, Expr(:parameters, prefs...))
end

function repl_token!(args, kws, tok::ReplToken)
    text = tok.text
    if !isnothing(tok.key)
        # an unquoted value is Julia code
        push!(kws, Expr(:kw, Symbol(tok.key), tok.quoted ? text : Meta.parse(text)))
        return
    end
    if tok.quoted
        push!(args, text)
        return
    end

    int = tryparse(Int, text)
    if !isnothing(int)
        push!(args, int)
    elseif occursin(r"^_\w+$", text)
        push!(args, QuoteNode(Symbol(text)))
    elseif startswith(text, ':') && length(text) > 1
        push!(args, QuoteNode(Symbol(text[2:end])))
    else
        push!(args, text)
    end
end

function repl_parse(line::AbstractString)
    tokens = repl_tokenize(line)
    if isempty(tokens)
        return nothing
    end

    cmd = popfirst!(tokens)
    if cmd.quoted || !isnothing(cmd.key)
        throw(ArgumentError("expected a command, got the pattern \"$(cmd.text)\"; " *
                            "type ? for help"))
    end

    args = []
    kws = []
    name = get(REPL_ALIASES, cmd.text, cmd.text)

    if name == "help"
        return Expr(:call, repl_help)
    elseif name == "load"
        if !isempty(tokens)
            throw(ArgumentError("$(cmd.text) doesn't take arguments"))
        end
        return Expr(:call, repl_load_tests)
    elseif name == "set"
        return repl_set(tokens)
    elseif name == "unset"
        return repl_unset(tokens)
    elseif name == "dry-run"
        push!(kws, Expr(:kw, :dry, true))
    elseif name == "run-failed"
        push!(args, fail)
    elseif name != "run" # `run` passes all its arguments as-is to `retest`
        throw(ArgumentError("unknown command \"$(cmd.text)\"; type ? for help"))
    end

    for tok in tokens
        repl_token!(args, kws, tok)
    end

    if isempty(kws)
        Expr(:call, repl_retest, args...)
    else
        Expr(:call, repl_retest, Expr(:parameters, kws...), args...)
    end
end
