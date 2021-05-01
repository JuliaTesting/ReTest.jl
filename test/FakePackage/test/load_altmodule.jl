include_string(Main.Load2, """
module AltModule
using ReTest
@testset "AltModule" begin end
end
""")
