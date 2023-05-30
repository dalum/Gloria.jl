module Parallax

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Event, Layer, Scene, Texture, Window, Resources, iskey

using Colors: @colorant_str

struct Controls <: AbstractObject end

mutable struct Object <: AbstractObject
    texture::Texture
    x::Float64
    y::Float64
    vx::Float64
    vy::Float64
    θ::Float64
    ω::Float64
end
Object(fname::String, args...) = Object(Texture(Resources(window), fname), args...)

function onevent!(::Controls, e::Event{:mousemotion})
    # Pan
    if Gloria.getmousestate().left
        for layer in object_layers
            layer.x += e.xrel * layer.scale
            layer.y += e.yrel * layer.scale
        end
    end
    # Zoom
    if Gloria.getmousestate().right
        for layer in object_layers
            layer.scale *= 1.001^e.yrel
            layer.x = (layer.x - width/2)*1.001^e.yrel + width/2
            layer.y = (layer.y - height/2)*1.001^e.yrel + height/2
            # truck/pedestal (moving the scene)
            # layer.x += e.xrel
            # layer.y += e.yrel
        end
    end
end

function onevent!(::Controls, e::Event{:mousebutton_down})
    (e.button == 1 || e.button == 3) && Gloria.setrelativemousemode!(true)
end

function onevent!(::Controls, e::Event{:mousebutton_up})
    (e.button == 1 || e.button == 3) && Gloria.setrelativemousemode!(false)
end

function onevent!(::Controls, e::Event{:key_down})
    iskey(e, "escape") && Gloria.quit!(window, e)
end

function update!(obj::Object; t::Float64, dt::Float64)
    obj.θ += obj.ω*dt
    obj.x += obj.vx*dt
    obj.y += obj.vy*dt
end

function render!(layer::Layer, obj::Object, frame::Int, fps::Float64)
    render!(layer, obj.texture, obj.x, obj.y, obj.θ)
end

# Setup

function main(; keepalive=true)
    @eval begin
        # const width, height = 1920, 1080
        const width, height = 800, 600
        const controls_layer = Layer([Controls()], show=false)
        const object_layers = [Layer(Object[], width/2, height/2, scale=1.5^-n) for n in 10:-1:0]
        const scene = Scene(controls_layer, object_layers...)
        const window = Window("Parallax", width, height, scene, fullscreen=false)

        for layer in object_layers
            push!(layer, [
                Object(
                    abspath(@__DIR__, "..", "assets", "sample.svg"),
                    (rand() - 0.5)*0.5width, (rand() - 0.5)*0.5height,
                    (rand() - 0.5)*width, (rand() - 0.5)*height,
                    rand()*360, (rand() - 0.5)*1080
                ) for _ in 1:50
            ]...)
        end
    end

    Gloria.run!(window)
    keepalive && wait(window)
end

# # precompile
# const dir = abspath(@__DIR__, "..", "precompile")
# const blacklist_import = [:Parallax, :unknown]
# const fnames = collect(filter(x->occursin(r"^precompile_.*\.jl$", x), readdir(dir)))
# const names = (fname->Symbol(match(r"^precompile_(.*)\.jl$", fname)[1])).(fnames)
# for name in names
#     name in blacklist_import && continue
#     try
#         @eval import $name
#     catch e
#         @warn "Failed import of: $name ($e)"
#     end
# end
# for fname in fnames
#     try
#         include(joinpath(dir, fname))
#         _precompile_()
#         catch e
#         @warn "Failed additional precompilation of: $fname ($e)"
#     end
# end

end # module
