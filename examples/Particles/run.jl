include(abspath(@__DIR__, "src", "Particles.jl"))
using .Particles
if Base.isinteractive()
    Particles.main(keepalive=false)
else
    Particles.main()
end
