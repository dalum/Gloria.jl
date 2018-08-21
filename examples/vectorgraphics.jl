module VectorGraphics

using Gloria: Gloria, Window, AbstractObject
using Gloria.Graphics: Texture
Gloria.init()

struct World <: AbstractObject
    objects::Vector{AbstractObject}
end

mutable struct Object <: AbstractObject
    x::Float64
    y::Float64
    θ::Float64
    texture::Gloria.Graphics.Texture
end
Object(x, y, θ, fname::String) = Object(x, y, θ, Texture(window, fname))

const width, height = 800, 600
const window = Window("Vector Graphics", (width, height), target_fps=30.0, target_speed=60.0, fullscreen=false)
const world = World(AbstractObject[Object(rand(1:width), rand(1:height), rand(1:360), abspath(@__DIR__, "assets", "sample.svg")) for _ in 1:100])
push!(window.scene_stack[end].objects, world)

function Gloria.update!(world::World; args...)
    for obj in world.objects
        Gloria.update!(obj; args...)
    end
end

function Gloria.update!(obj::Object; args...)
    obj.θ += 180*args[:dt]
end

function Gloria.render!(window::Window, world::World; args...)
    Gloria.Graphics.setcolor!(window, 100, 100, 100, 255)
    Gloria.Graphics.clear!(window)
    for obj in world.objects
        Gloria.render!(window, obj; args...)
    end
    Gloria.Graphics.present!(window)
end

function Gloria.render!(window::Window, obj::Object; args...)
    Gloria.render!(window, obj.texture, obj.x, obj.y, obj.θ)
end

end # module
