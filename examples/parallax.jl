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
    if Gloria.getmousestate().left
        for layer in [object_layer1, object_layer2, object_layer3]
            layer.origin_x += e.rel_x * layer.scale
            layer.origin_y += e.rel_y * layer.scale
        end
    end
    if Gloria.getmousestate().right
        for layer in scene.layers
            layer.scale *= 1.001^e.rel_y
        end
    end
end

function onevent!(::Controls, ::Val{:mousebutton_down}, e::Event)
    e.button == 1 && Gloria.setrelativemousemode!(true)
end

function onevent!(::Controls, ::Val{:mousebutton_up}, e::Event)
    e.button == 1 && Gloria.setrelativemousemode!(false)
end

function update!(obj::Object; dt, args...)
    obj.θ += obj.ω*dt
    obj.x += obj.vx*dt
    obj.y += obj.vy*dt
    # abs(obj.x) > width / 2 && (obj.vx *= -1; obj.ω *= -1; obj.x += obj.vx*dt)
    # abs(obj.y) > height / 2 && (obj.vy *= -1; obj.ω *= -1; obj.y += obj.vy*dt)
end

function render!(layer::Layer, obj::Object; args...)
    render!(layer, obj.texture, obj.x, obj.y, angle=obj.θ)
end

# Setup

const width, height = 800, 600
const controls_layer = Layer([Controls()], show=false)
const object_layer1 = Layer(Object[], width/2, height/2, [1. 0.; 0. -1.], scale=.3)
const object_layer2 = Layer(Object[], width/2, height/2, [0. 1.; 1. 0.], scale=.5)
const object_layer3 = Layer(Object[], width/2, height/2, [1. 0.; 0. -1.], scale=1.)
const scene = Scene(controls_layer, object_layer1, object_layer2, object_layer3)
const window = Window("Parallax", width, height, scene)

append!(object_layer1, [Object(abspath(@__DIR__, "assets", "sample.svg"), 100.0i, 20.0i, 0.0, 0.0, rand()*360, rand()*(360 - 180)) for i in 0:10])
append!(object_layer2, [Object(abspath(@__DIR__, "assets", "sample.svg"), 100.0i, 20.0i, 0.0, 0.0, rand()*360, rand()*(360 - 180)) for i in 0:10])
append!(object_layer3, [Object(abspath(@__DIR__, "assets", "sample.svg"), 100.0i, 0.0, 0.0, 0.0, rand()*360, rand()*(360 - 180)) for i in 0:10])

Gloria.run!(window, target_render_speed = 60.0, target_update_speed = 60.0)
# wait(window)

end # module
