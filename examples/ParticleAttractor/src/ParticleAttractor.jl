module ParticleAttractor

import Gloria: onevent!, render!, update!
using Gloria: Gloria, Window, AbstractObject, Event, Layer, Scene

struct Controls <: AbstractObject end

struct World <: AbstractObject
    action::Ref{Int}
    objects::Vector{AbstractObject}
end

mutable struct Particle <: AbstractObject
    x::Float64
    y::Float64
    vx::Float64
    vy::Float64
    accel::Float64
    resistance::Float64
end

const width, height = 800, 600
const world = World(
    Ref{Int}(false),
    AbstractObject[Particle(
        width/2 + rand() - 0.5, height/2 + rand() - 0.5,
        0, 0,
        200, 0.5
    ) for _ in 1:15000]
)
const controls_layer = Layer([Controls()], show=false)
const scene = Scene(Layer([world]), controls_layer)
const window = Window("Particle Attractor", width, height, scene, fullscreen=false)


function Gloria.onevent!(::Controls, e::Event{:mousebutton_down})
    world.action[] += 2 - e.button
end
function Gloria.onevent!(::Controls, e::Event{:mousebutton_up})
    world.action[] -= 2 - e.button
end
function Gloria.onevent!(::Controls, e::Event{:key_down})
    Gloria.iskey(e, "escape") && Gloria.quit!(window, e)
end

function Gloria.update!(world::World, ::Gloria.AbstractLayer, t, dt)
    mouse = Gloria.getmousestate()

    # for obj1 in world.objects, obj2 in world.objects
    #     Gloria.update!(obj1, obj2, t, dt)
    # end
    for obj in world.objects
        Gloria.update!(obj, mouse, t, dt)
    end
end

function Gloria.update!(obj::Particle, mouse, t, dt)
    Δx, Δy = mouse.x - obj.x, mouse.y - obj.y
    obj.vx += dt * (world.action[] * obj.accel * Δx / (Δx^2 + Δy^2 + 1) - obj.vx * obj.resistance)
    obj.vy += dt * (world.action[] * obj.accel * Δy / (Δx^2 + Δy^2 + 1) - obj.vy * obj.resistance)
    obj.x += obj.vx
    obj.y += obj.vy
    obj.x = mod(obj.x, width)
    obj.y = mod(obj.y, height)
end

@fastmath function Gloria.update!(obj1::Particle, obj2::Particle, t, dt)
    Δx, Δy = obj1.x - obj2.x, obj1.y - obj2.y
    obj1.vx += 1e-4 * dt * (obj1.accel * Δx / (Δx^2 + Δy^2 + 1))
    obj1.vy += 1e-4 * dt * (obj1.accel * Δy / (Δx^2 + Δy^2 + 1))
end

function Gloria.render!(::Layer, obj::World, frame, fps)
    Gloria.render!(window, obj, frame, fps)
end
function Gloria.render!(window::Window, world::World, frame, fps)
    Gloria.setcolor!(window, 100, 100, 100, 255)
    Gloria.clear!(window)
    for obj in world.objects
        Gloria.render!(window, obj, frame, fps)
    end
    Gloria.present!(window)
end

function Gloria.render!(window::Window, obj::Particle, frame, fps)
    Gloria.setcolor!(window, 255, 255, 255, 255)
    Gloria.drawpoint!(window, Int(floor(obj.x)), Int(floor(obj.y)))
    # Gloria.Graphics.fillrect(window, Int(floor(obj.x)), Int(floor(obj.y)), 20, 20)
    # Gloria.Graphics.setcolor(window, 0, 0, 0, 255)
    # Gloria.Graphics.drawrect(window, Int(floor(obj.x)), Int(floor(obj.y)), 20, 20)
end

function main(; keepalive=true)
    Gloria.run!(window)
    keepalive && wait(window)
end

end # module
