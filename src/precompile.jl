import PrecompileTools: @setup_workload, @compile_workload

module _ReTestPrecompile
const x = 1
end

module _ReTestPrecompileTests
import .._ReTestPrecompile
using ..ReTest

@testset "precompilation workload" begin
    @test _ReTestPrecompile.x == 1
    @test false
end

end # _ReTestPrecompileTestsModule


@setup_workload begin
    stderr_pipe = Pipe()
    stdout_pipe = Pipe()

    @compile_workload begin
        try
            redirect_stdio(; stderr=stderr_pipe, stdout=stdout_pipe) do
                retest(_ReTestPrecompile, _ReTestPrecompileTests; recursive=false, stats=true, spin=true)
            end
        catch ex
            close(stderr_pipe.in)
            close(stdout_pipe.in)

            if !(ex isa Test.TestSetException)
                stdout_str = read(stdout_pipe, String)
                stderr_str = read(stderr_pipe, String)

                @error "Precompilation failed, this is the captured stdout ($(length(stdout_str)) chars):"
                println(stdout_str)

                @error "And this is the captured stderr ($(length(stderr_str)) chars):"
                println(stderr_str)

                rethrow()
            end
        finally
            empty!(ReTest.TESTED_MODULES)
            close(stderr_pipe)
            close(stdout_pipe)
        end
    end
end
