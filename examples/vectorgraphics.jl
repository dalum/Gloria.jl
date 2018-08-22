module VectorGraphics

import Gloria: onevent!, render!, update!

using Gloria: Gloria, AbstractObject, Scene, Window
using Gloria.Graphics: Texture
using Gloria.Events: Event
Gloria.init()

using Colors: @colorant_str

struct Controls <: AbstractObject end

mutable struct Object <: AbstractObject
    texture::Gloria.Graphics.Texture
    x::Float64
    y::Float64
    vx::Float64
    vy::Float64
    θ::Float64
    ω::Float64
end
Object(fname::String, args...) = Object(Texture(window, fname), args...)

function onevent!(::Controls, ::Val{:mousemotion}, e::Event)
    if Gloria.Mouse.getmousestate().left
        scene.camera_x -= e.rel_x
        scene.camera_y -= e.rel_y
    end
end

function onevent!(::Controls, ::Val{:mousebutton_down}, e::Event)
    e.button == 1 && Gloria.Mouse.setrelative(true)
end

function onevent!(::Controls, ::Val{:mousebutton_up}, e::Event)
    e.button == 1 && Gloria.Mouse.setrelative(false)
end

function update!(obj::Object; dt, args...)
    obj.θ += obj.ω*dt
    obj.x += obj.vx*dt
    obj.y += obj.vy*dt
    abs(obj.x) > width / 2 && (obj.vx *= -1; obj.ω *= -1; obj.x += obj.vx*dt)
    abs(obj.y) > height / 2 && (obj.vy *= -1; obj.ω *= -1; obj.y += obj.vy*dt)
end

function render!(window::Window, obj::Object; args...)
    render!(window, obj.texture, obj.x, obj.y, obj.θ)
end

# Setup

const width, height = 800, 600
const window = Window("Vector Graphics", width, height, target_fps=60.0, target_speed=60.0)
const scene = Scene()

push!(scene, Controls())
append!(scene, [Object(abspath(@__DIR__, "assets", "sample.svg"),
                       (rand() - 0.5)*width, (rand() - 0.5)*height,
                       (rand() - 0.5)*width, (rand() - 0.5)*height,
                       rand()*360, (rand() - 0.5)*1080) for _ in 1:50])

push!(window, scene)

Gloria.start(keepalive=false)

end # module
