include(abspath(@__DIR__, "src", "Inside.jl"))
using .Inside
if Base.isinteractive()
    Inside.main(keepalive=false)
else
    Inside.main()
end
