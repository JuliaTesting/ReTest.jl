using Documenter, ReTest

makedocs(sitename = "ReTest.j",
         modules = [ReTest, ReTest.InlineTest])


deploydocs(
    repo = "github.com/JuliaTesting/ReTest.jl.git",
)
