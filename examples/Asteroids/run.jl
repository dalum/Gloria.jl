include(abspath(@__DIR__, "src", "Asteroids.jl"))
using .Asteroids
if Base.isinteractive()
    Asteroids.main(keepalive=false)
else
    Asteroids.main()
end
