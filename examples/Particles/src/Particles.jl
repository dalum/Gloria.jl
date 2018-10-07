module Particles

using StaticArrays

import Gloria: onevent!, render!, update!, after_update!
using Gloria: Gloria, AbstractObject, AbstractShape, Audio, Event, Layer, Scene, Texture, Window,
    add!, kill!, play!,
    getmousestate, isalive, isbutton, iskey, ispressed, tocoordinates

using Gloria.Shapes: Shapes, Polygon, Polyline, Vertex, rotate

using Gloria.Physics
using Gloria.Physics: Physics

using Colors: RGB, @colorant_str

##################################################
# Types
##################################################

mutable struct Controls <: AbstractObject end

struct Floor <: AbstractObject end
Physical{Floor}(x, y, θ) = Physical(Floor(), Shapes.HalfSpace(Vertex(0., 0), SVector(1., 0.) |> rotate(θ)), position=SVector(x, y), static=true)

mutable struct Particle <: AbstractObject
    color::RGB
end
Physical{Particle}(color, x, y, n=5) = Physical(
    Particle(color),
    Polygon((r = 0rand(); Vertex((10 + r)cos(2π*i/n), (10 + r)sin(2π*i/n))) for i in 0:(n-1)),
    position=SVector(x, y),
    mass=5.,
    angularmass=5000.)

##################################################
# Events
##################################################

function onevent!(::Controls, e::Event{:key_down})
    iskey(e, "escape") && Gloria.quit!(window, e)
    iskey(e, "space") && (window.loops["update"].state[:paused] = !window.loops["update"].state[:paused])
    iskey(e, "backspace") && (window.loops["update"].state[:step] = !window.loops["update"].state[:step])
end

function onevent!(::Controls, e::Event{:mousebutton_down})
    if isbutton(e, "left")
        add!(object_layer, Physical{Particle}(colorant"#000", tocoordinates(object_layer, e.x, e.y)...))
    end
end

##################################################
# Update
##################################################

function after_update!(self::Physical{Particle}, ::Layer, t, dt)
    Physics.setvelocity!(self, Physics.velocity(self) + SVector(0., 500.0*dt))
end

Physics.restitution(::Physical{Particle}, ::Physical{Particle}) = 1.0
Physics.restitution(::Physical{Floor}, ::Physical{Particle}) = 1.0
Physics.restitution(::Physical{Particle}, ::Physical{Floor}) = 1.0

##################################################
# Render
##################################################

function render!(layer::Layer, self::Physical{Particle}, frame::Int, fps::Float64)
    render!(layer, self.shape, Physics.position(self)..., Physics.angle(self)..., color=self.color)
end

function render!(layer::Layer, self::Union{Physical{Floor}}, frame::Int, fps::Float64)
    render!(layer, self.shape, Physics.position(self)..., Physics.angle(self)..., color=colorant"#000")
end

###

##################################################
# Setup
##################################################

const width, height = 800, 600
const controls_layer = Layer([Controls()], width/2, height/2)

const object_layer = Layer(Physical[], width/2, height/2)
const collision_layer = CollisionLayer(object_layer, object_layer, width/2, height/2, show=true)

const scene = Scene(controls_layer, object_layer, collision_layer, color=colorant"#D0D0D0")
const window = Window("Inside", width, height, scene, fullscreen=false)

const keyboard = Gloria.KeyboardState()

add!(object_layer, Physical{Floor}(0., 200., -90.))
add!(object_layer, Physical{Floor}(0., -200., 90.))
add!(object_layer, Physical{Floor}(-300., 0., 0.))
add!(object_layer, Physical{Floor}(300., 0., 180.))
for _ in 1:20
    add!(object_layer, Physical{Particle}(colorant"#000", rand(-width/2:width/2), rand(-height/2:(-height/2+200)), rand(3:7)))
end

function main(;keepalive=true)
    Gloria.run!(window)
    if keepalive
        wait(window)
    end
end

end # module
