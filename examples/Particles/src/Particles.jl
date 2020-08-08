module Particles

using StaticArrays

import Gloria: onevent!, render!, update!, after_update!
using Gloria: Gloria,
    AbstractObject, AbstractShape,
    Event, Font, Layer, Resources, Scene, Window,
    add!, kill!, play!,
    getmousestate, isalive, isbutton, iskey, ispressed, tocoordinates,
    text

using Gloria.Shapes: Shapes, Polygon, Vertex, rotate

using Gloria.Physics
using Gloria.Physics: Physics

using Colors: RGB, @colorant_str

##################################################
# Types
##################################################

mutable struct Controls <: AbstractObject end

struct Floor <: AbstractObject end
function Physical{Floor}(x, y, θ)
    Physical(
        Floor(),
        Shapes.HalfSpace(
            Vertex(0., 0),
            SVector(1., 0.) |> rotate(θ)
        ),
        position = SVector(x, y),
        static = true
    )
end

struct Plateau <: AbstractObject end
function Physical{Plateau}(x, y, w, h, θ)
    Physical(
        Plateau(),
        Shapes.Polygon(
            Vertex(-w/2., h/2),
            Vertex(w/2., h/2),
            Vertex(w/2., -h/2),
            Vertex(-w/2., -h/2)
        ),
        position = SVector(x, y),
        angle = SVector(θ),
        static = true
    )
end

mutable struct Particle <: AbstractObject
    color::RGB
end
function Physical{Particle}(color, x, y, n=4)
    vertices = map(0:n-1) do i
        Vertex(20cos(2π*i/n), 20sin(2π*i/n))
    end
    Physical(
        Particle(color),
        Polygon(vertices),
        position = SVector(x, y),
        mass = 5.0,
        angularmass = 1500.0,
    )
end

##################################################
# Events
##################################################

function onevent!(::Controls, e::Event{:key_down})
    iskey(e, "escape") && Gloria.quit!(window, e)
    iskey(e, "space") && (window.loops["update"].paused = !window.loops["update"].paused)
end

function onevent!(::Controls, e::Event{:mousebutton_down})
    if isbutton(e, "left")
        add!(object_layer, Physical{Particle}(colorant"#000", tocoordinates(object_layer, e.x, e.y)...))
    end
    if isbutton(e, "right")
        add!(object_layer, Physical{Particle}(colorant"#000", tocoordinates(object_layer, e.x, e.y)..., 6))
    end
end

##################################################
# Update
##################################################

function after_update!(self::Physical{Particle}, ::Layer, t, dt)
    Physics.applyimpulse!(self, SVector(0., Physics.mass(self)[1]*500.0*dt), Physics.position(self))
end

Physics.restitution(::Physical{Particle}, ::Physical{Particle}) = 0.1
Physics.restitution(::Physical{Floor}, ::Physical{Particle}) = 0.1
Physics.restitution(::Physical{Plateau}, ::Physical{Particle}) = 0.1
Physics.dynamicfriction(::Union{Physical{Floor}, Physical{Plateau}, Physical{Particle}}, ::Physical{Particle}) = 0.4
Physics.staticfriction(::Union{Physical{Floor}, Physical{Plateau}, Physical{Particle}}, ::Physical{Particle}) = 0.6

##################################################
# Render
##################################################

function render!(layer::Layer, self::Physical{Particle}, frame::Int, fps::Float64)
    render!(layer, self.shape, Physics.position(self)..., Physics.angle(self)..., color=self.color)
end

function render!(layer::Layer, self::Union{Physical{Floor}, Physical{Plateau}}, frame::Int, fps::Float64)
    render!(layer, self.shape, Physics.position(self)..., Physics.angle(self)..., color=colorant"#000")
end

function render!(layer::Layer, ::Controls, frame, fps)
    render!(layer, text(font_noto, "FPS: $(round(fps; digits=1))", color=colorant"#000"; halign=1.0), width, 0., 0.)
end

##################################################
# Setup
##################################################

function main(; keepalive=true)
    @eval begin
        const width, height = 800, 600
        const controls_layer = Layer([Controls()])

        const object_layer = Layer(AbstractObject[Physics.CollisionSolver{2}()], width/2, height/2)

        const scene = Scene(object_layer, controls_layer, color=colorant"#D0D0D0")

        const window = Window("Particles", width, height, scene, fullscreen=false)

        const resources = Resources(window)
        const keyboard = Gloria.KeyboardState()

        const font_noto = Font(resources, abspath(@__DIR__, "..", "assets", "NotoSans-Black.ttf"), fontsize=24)

        add!(object_layer, Physical{Floor}(0., 200., -90.))
        add!(object_layer, Physical{Floor}(0., -200., 80.))
        add!(object_layer, Physical{Floor}(-300., 0., 20.))
        add!(object_layer, Physical{Floor}(300., 0., 190.))
        add!(object_layer, Physical{Plateau}(-200., 0., 300., 20., 10.))
        add!(object_layer, Physical{Plateau}(50., -150., 300., 20., -10.))
        add!(object_layer, Physical{Plateau}(100., 150., 300., 20., -10.))
        for _ in 1:0
            add!(object_layer, Physical{Particle}(colorant"#000", rand(-width/2:width/2), rand(-height/2:(-height/2+200)), rand(3:7)))
        end

    end

    Gloria.run!(window)
    keepalive && wait(window)
end

end # module
