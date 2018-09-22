module Particles

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, AbstractShape, Audio, Event, Layer, Scene, Texture, Window,
    add!, kill!, play!,
    getmousestate, isalive, isbutton, iskey, ispressed, tocoordinates

using Gloria.Shapes: Line, Point, circle, inside, intersects

using Gloria.Physics

using Colors: RGB, @colorant_str

##################################################
# Types
##################################################

mutable struct Controls <: AbstractObject end

struct Floor <: AbstractObject end
Physical{Floor}(y) = Physical(Floor(), Line(Point(-width/2, 0.), Point(width/2, 0.)), y=y, static=true)

mutable struct Particle <: AbstractObject
    color::RGB
end
Physical{Particle}(color, x, y) = Physical(Particle(color), circle(Point(0., 0.), 50., samples=12), x=x, y=y)

##################################################
# Events
##################################################

function onevent!(::Controls, e::Event{:key_down})
    iskey(e, "escape") && Gloria.quit!(window, e)
    iskey(e, "space") && (window.loops["update"].state[:step] = true)
end

function onevent!(::Controls, e::Event{:mousebutton_down})
    if isbutton(e, "left")
        add!(object_layer, Physical{Particle}(colorant"#000", tocoordinates(object_layer, e.x, e.y)...))
    end
end

##################################################
# Update
##################################################

function update!(self::Physical{Particle}, t, dt)
    self.vy += 500.0*dt
end

##################################################
# Render
##################################################

function render!(layer::Layer, self::Physical{Particle}, frame::Int, fps::Float64)
    render!(layer, self.shape, self.x, self.y, self.θ, color=self.color)
    # for obj in filter(obj -> obj !== self, object_layer)
    #     shape, lines = Physics.timecapture(self, obj)
    #     render!(layer, shape, 0., 0., 0., color=colorant"#F00")
    #     render!(layer, lines, 0., 0., 0., color=colorant"#F00")
    #     shape, lines = Physics.timecapture(obj, self)
    #     render!(layer, shape, 0., 0., 0., color=colorant"#F00")
    #     render!(layer, lines, 0., 0., 0., color=colorant"#F00")
    # end
end

function render!(layer::Layer, self::Physical{Floor}, frame::Int, fps::Float64)
    render!(layer, self.shape, self.x, self.y, self.θ, color=colorant"#000")
end

# function render!(layer::Layer, self::Controls, frame::Int, fps::Float64)
#     if count(x->x isa Physical{Particle}, object_layer) > 0
#         x, y = Gloria.Physics.centerofmass(filter(x->x isa Physical{Particle}, object_layer))
#         render!(layer, Circle(0., 0., 10.), x, y, 0., color=colorant"#F00")
#     end
# end

###

##################################################
# Setup
##################################################

const width, height = 800, 600
const controls_layer = Layer([Controls()], width/2, height/2)

const object_layer = Layer(Physical[], width/2, height/2)

const scene = Scene(controls_layer, object_layer, color=colorant"#D0D0D0")
const window = Window("Inside", width, height, scene, fullscreen=false)

const keyboard = Gloria.KeyboardState()

add!(object_layer, Physical{Floor}(200.))
for _ in 1:1
    add!(object_layer, Physical{Particle}(colorant"#000", rand(-width/2:width/2), rand(-height/2:(-height/2+200))))
end

function main(;keepalive=true)
    Gloria.run!(window)
    if keepalive
        wait(window)
    end
end

end # module
