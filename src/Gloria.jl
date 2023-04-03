"""
Gloria.
"""
module Gloria

using Colors
using DataStructures: CircularBuffer, OrderedSet
using FileIO
import SimpleDirectMediaLayer as SDL

const fps=120

function __init__()
    SDL.SDL_GL_SetAttribute(SDL.SDL_GL_MULTISAMPLEBUFFERS, 1)
    SDL.SDL_GL_SetAttribute(SDL.SDL_GL_MULTISAMPLESAMPLES, 4)
    SDL.init()
end

abstract type AbstractObject end

rgba_fromcolor(color::Colors.Color) = round(Int, color.r * 255), round(Int, color.g * 255), round(Int, color.b * 255), round(Int, color.alpha * 255)
rgba_fromcolor(color::Colors.Color3) = round(Int, color.r * 255), round(Int, color.g * 255), round(Int, color.b * 255), round(Int, 255)
rgba_fromcolor(color::Colors.Color3, a::Int) = round(Int, color.r * 255), round(Int, color.g * 255), round(Int, color.b * 255), a

include("window.jl")
include("resources.jl")
include("texture.jl")
include("text.jl")

include("Shapes.jl")
using .Shapes
using .Shapes: rotate, translate

include("parseevent.jl")
include("loops.jl")
include("mouse.jl")
include("keyboard.jl")
include("audio.jl")

include("graphics.jl")

include("model.jl")

# Submodules
include("Physics.jl")

end # module
