using Documenter, ReTest

makedocs(; sitename = "ReTest.jl",
         modules = [ReTest, ReTest.InlineTest],
         warnonly=[:missing_docs])


deploydocs(
    repo = "github.com/JuliaTesting/ReTest.jl.git",
    push_preview = true,
)
