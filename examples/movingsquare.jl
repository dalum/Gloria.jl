module ParticleAttractor

using Gloria: Gloria, Window, AbstractObject
Gloria.init()

struct World <: AbstractObject
    active::Vector{Bool}
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
const window = Window("Particle Attractor", (width, height), target_fps=30.0, target_speed=30.0, fullscreen=false)
const world = World([false], AbstractObject[Particle(rand(0:width), rand(0:height), 0, 0, 100, 0.5) for _ in 1:15000])
push!(window.scene_stack[end].objects, world)

Gloria.Mouse.buttondown(button, x, y) = button == 1 && (world.active[] = true)
Gloria.Mouse.buttonup(button, x, y) = button == 1 && (world.active[] = false)

function Gloria.update(world::World; args...)
    mouse = Gloria.Mouse.getmousestate()

    # for obj1 in world.objects, obj2 in world.objects
    #     Gloria.update(obj1, obj2; args...)
    # end
    for obj in world.objects
        Gloria.update(obj; mouse=mouse, args...)
    end
end

function Gloria.update(obj::Particle; mouse, t, dt)
    Δx, Δy = mouse.x - obj.x, mouse.y - obj.y
    obj.vx += dt * (world.active[] * obj.accel * Δx / (Δx^2 + Δy^2 + 1) - obj.vx * obj.resistance)
    obj.vy += dt * (world.active[] * obj.accel * Δy / (Δx^2 + Δy^2 + 1) - obj.vy * obj.resistance)
    obj.x += obj.vx
    obj.y += obj.vy
    obj.x = mod(obj.x, width)
    obj.y = mod(obj.y, height)
end

function Gloria.update(obj1::Particle, obj2::Particle; t, dt)
    Δx, Δy = obj1.x - obj2.x, obj1.y - obj2.y
    obj1.vx += dt * (obj1.accel * Δx / (Δx^2 + Δy^2 + 1)) / 1000
    obj1.vy += dt * (obj1.accel * Δy / (Δx^2 + Δy^2 + 1)) / 1000
end

function Gloria.render(window::Window, world::World; args...)
    Gloria.Graphics.setcolor(window, 100, 100, 100, 255)
    Gloria.Graphics.clear(window)
    for obj in world.objects
        Gloria.render(window, obj; args...)
    end
    Gloria.Graphics.present(window)
end

function Gloria.render(window::Window, obj::Particle; frame, fps)
    Gloria.Graphics.setcolor(window, 255, 255, 255, 255)
    Gloria.Graphics.drawpoint(window, (Int(floor(obj.x)), Int(floor(obj.y))))
    # Gloria.Graphics.fillrect(window, Int(floor(obj.x)), Int(floor(obj.y)), 20, 20)
    # Gloria.Graphics.setcolor(window, 0, 0, 0, 255)
    # Gloria.Graphics.drawrect(window, Int(floor(obj.x)), Int(floor(obj.y)), 20, 20)
end

end # module
