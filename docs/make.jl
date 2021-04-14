using Documenter, ReTest

makedocs(sitename = "ReTest.jl",
         modules = [ReTest, ReTest.InlineTest])


deploydocs(
    repo = "github.com/JuliaTesting/ReTest.jl.git",
)
