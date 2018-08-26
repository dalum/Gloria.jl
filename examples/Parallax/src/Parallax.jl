module Parallax

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Event, Layer, Scene, Texture, Window

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
Object(fname::String, args...) = Object(Texture(window, fname), args...)

function onevent!(::Controls, ::Val{:mousemotion}, e::Event)
    # Pan
    if Gloria.getmousestate().left
        for layer in object_layers
            layer.origin_x += e.rel_x * layer.scale
            layer.origin_y += e.rel_y * layer.scale
        end
    end
    # Zoom
    if Gloria.getmousestate().right
        for layer in object_layers
            layer.scale *= 1.001^e.rel_y
            layer.origin_x = (layer.origin_x - width/2)*1.001^e.rel_y + width/2
            layer.origin_y = (layer.origin_y - height/2)*1.001^e.rel_y + height/2
        end
    end
end

function onevent!(::Controls, ::Val{:mousebutton_down}, e::Event)
    (e.button == 1 || e.button == 3) && Gloria.setrelativemousemode!(true)
end

function onevent!(::Controls, ::Val{:mousebutton_up}, e::Event)
    (e.button == 1 || e.button == 3) && Gloria.setrelativemousemode!(false)
end

function onevent!(::Controls, ::Val{:key_down}, e::Event)
    if e.scancode == Gloria.SDL.SCANCODE_ESCAPE
        Gloria.quit!(window, e)
    end
end

function update!(obj::Object; t::Float64, dt::Float64)
    obj.θ += obj.ω*dt
    obj.x += obj.vx*dt
    obj.y += obj.vy*dt
end

function render!(layer::Layer, obj::Object; frame::Int, fps::Float64)
    render!(layer, obj.texture, obj.x, obj.y, angle=obj.θ)
end

# Setup

# const width, height = 1920, 1080
const width, height = 800, 600
const controls_layer = Layer([Controls()], show=false)
const object_layers = [Layer(Object[], width/2, height/2, [1. 0.; 0. -1.], scale=1.5^-n) for n in 10:-1:0]
const scene = Scene(controls_layer, object_layers...)
const window = Window("Parallax", width, height, scene, fullscreen=false)

for layer in object_layers
    append!(layer, [Object(abspath(@__DIR__, "..", "assets", "sample.svg"), 0.0, 0.0, (randn() - 0.5)*20, (randn() - 0.5)*20, (randn() - 0.5)*360, (randn() - 0.5)*60) for i in 0:100])
end

function main()
    Gloria.run!(window, target_render_speed = 60.0, target_update_speed = 60.0)
    wait(window)
end

# precompile
import Cairo, Compat, DelimitedFiles, LinearAlgebra, Logging, Random, Rsvg, SimpleDirectMediaLayer
for fname in filter(x->occursin(r"^precompile_.*\.jl$", x), readdir(abspath(@__DIR__, "..", "precompile")))
    include(abspath(@__DIR__, "..", "precompile", fname))
    _precompile_()
end

end # module
