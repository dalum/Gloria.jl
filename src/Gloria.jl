"""
Gloria.
"""
module Gloria

using SimpleDirectMediaLayer, FileIO, DataStructures
const SDL = SimpleDirectMediaLayer

function __init__()
    SDL.GL_SetAttribute(SDL.GL_MULTISAMPLEBUFFERS, 1)
    SDL.GL_SetAttribute(SDL.GL_MULTISAMPLESAMPLES, 4)
    SDL.init()
end

using Colors

abstract type AbstractResource end
abstract type AbstractObject end

include("window.jl")
include("texture.jl")
include("shape.jl")
include("events.jl")
include("loops.jl")
include("graphics.jl")
include("mouse.jl")
include("keyboard.jl")
include("audio.jl")

include("physics.jl")

end # module
