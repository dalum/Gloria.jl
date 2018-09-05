@async begin
    sleep(0.1)
    @eval begin
        if Base.isinteractive()
            using Revise
        end
        include(abspath(@__DIR__, "src", "Asteroids.jl"))
        using .Asteroids
        if Base.isinteractive()
            Asteroids.main(keepalive=false)
        else
            Asteroids.main()
        end
    end
end
