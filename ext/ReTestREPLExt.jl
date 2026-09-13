module ReTestREPLExt

import REPL
import TOML
using Base.CoreLogging: CoreLogging
using ReplMaker: initrepl, FunctionCompletionProvider
using ReTest: ReTest
using TestEnv: TestEnv


function load_tests()
    project = Base.active_project()
    name = get(TOML.parsefile(project), "name", nothing)
    if isnothing(name)
        error("the active project ($project) is not a package")
    end
    # must be computed before activating, which changes the active project
    testdir = joinpath(dirname(project), "test")
    testpath = joinpath(testdir, "$(name)Tests.jl")
    if !isfile(testpath)
        error("no test file found at $testpath")
    end

    @info "Switching to test environment..."
    TestEnv.activate()
    try
        @info "Switched to $(Base.active_project())"
        keep_preferences(project, testdir)
        ReTest.load(testpath)
        @info "Loaded $testpath"
    catch
        # leave the session as it was, so `load` can be tried again
        Base.set_active_project(project)
        undo_keep_preferences()
        rethrow()
    end
end

# The LOAD_PATH entry added by `keep_preferences`, if any
const pushed_env = Ref{Union{Nothing, String}}(nothing)

# Preferences set in the temporary test environment would be lost with it, so write
# them to the project `Pkg.test` reads them from, and put it on the load path to read
# them back (the active temporary project doesn't inherit from it)
function keep_preferences(project, testdir)
    testproject = nothing
    for name in Base.project_names
        file = joinpath(testdir, name)
        if isfile(file)
            testproject = file
            break
        end
    end
    if !isnothing(testproject)
        project = testproject
    end
    ReTest.preferences_project = project
    env = dirname(project)
    if !(env in Base.LOAD_PATH)
        push!(Base.LOAD_PATH, env)
        pushed_env[] = env
    end
end

function undo_keep_preferences()
    ReTest.preferences_project = nothing
    env = pushed_env[]
    if !isnothing(env)
        filter!(!=(env), Base.LOAD_PATH)
        pushed_env[] = nothing
    end
end

# ReplMaker warns that `}` is already bound by the REPL's bracket insertion, which it
# preserves anyway when not at the start of the line: drop that message, keep the rest
struct DropKeyWarning{L<:CoreLogging.AbstractLogger} <: CoreLogging.AbstractLogger
    parent::L
end

CoreLogging.shouldlog(l::DropKeyWarning, args...) = CoreLogging.shouldlog(l.parent, args...)
CoreLogging.min_enabled_level(l::DropKeyWarning) = CoreLogging.min_enabled_level(l.parent)
CoreLogging.catch_exceptions(l::DropKeyWarning) = CoreLogging.catch_exceptions(l.parent)

function CoreLogging.handle_message(l::DropKeyWarning, level, message, args...; kwargs...)
    if level == CoreLogging.Warn && startswith(string(message), "REPL key ")
        return nothing
    end
    CoreLogging.handle_message(l.parent, level, message, args...; kwargs...)
end

function init_repl_mode(repl)
    CoreLogging.with_logger(DropKeyWarning(CoreLogging.current_logger())) do
        initrepl(ReTest.repl_parse;
                 repl,
                 prompt_text = "retest> ",
                 prompt_color = :magenta,
                 start_key = '}',
                 mode_name = :retest,
                 completion_provider = FunctionCompletionProvider(ReTest.repl_completions),
                 startup_text = false)
    end
end

function __init__()
    ReTest.repl_load_tests_hook = load_tests
    if isdefined(Base, :active_repl) && Base.active_repl isa REPL.LineEditREPL
        init_repl_mode(Base.active_repl)
    elseif isinteractive()
        Base.atreplinit(init_repl_mode)
    end
end

end
