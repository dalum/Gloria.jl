module Inside

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, AbstractShape, Audio, Circle, Event, Layer, Line, Lines, Point, Polygon, Scene, Texture, Window,
    add!, kill!, play!,
    getmousestate, inside, intersects, isalive, isbutton, iskey, ispressed, tocoordinates

using Colors: RGB, @colorant_str

##################################################
# Types
##################################################

mutable struct Controls <: AbstractObject end

abstract type InsideObject <: AbstractObject end

mutable struct Shape <: InsideObject
    shape::AbstractShape
    color::RGB
    x::Float64
    y::Float64
    θ::Float64
    active::Bool
end
Shape(shape, x, y, θ) = Shape(shape, colorant"#303030", x, y, θ, false)

##################################################
# Events
##################################################

function onevent!(::Controls, e::Event{:key_down})
    iskey(e, "escape") && Gloria.quit!(window, e)
end

function onevent!(self::Shape, e::Event{:mousemotion})
    if self.active
        if getmousestate().left
            self.x += e.rel_x
            self.y += e.rel_y
        end
    end
end

function onevent!(self::Shape, e::Event{:mousebutton_down})
    if inside(Gloria.transform(self.shape, self.x, self.y, self.θ), Point(tocoordinates(shape_layer, e.x, e.y)...), 90.) && isbutton(e, "left")
        self.active = true
    end
end

function onevent!(self::Shape, e::Event{:mousebutton_up})
    if isbutton(e, "left")
        self.active = false
    end
end

##################################################
# Update
##################################################

function update!(self::Shape; t, dt)
    if any(shape -> inside(Gloria.transform(self.shape, self.x, self.y, self.θ), Gloria.transform(shape.shape, shape.x, shape.y, shape.θ), 90), filter(x->x ≢ self, shape_layer))
        self.color = colorant"#FA00FA"
    else
        self.color = colorant"#303030"
    end
end

##################################################
# Render
##################################################

function render!(layer::Layer, self::Shape; frame::Int, fps::Float64)
    render!(layer, self.shape, self.x, self.y, self.θ, color=self.color)
end

###

##################################################
# Setup
##################################################

# const width, height = 1920, 1080
const width, height = 800, 600
const controls_layer = Layer([Controls()], show=false)

const shape_layer = Layer(Shape[], width/2, height/2)

const scene = Scene(controls_layer, shape_layer, color=colorant"#D0D0D0")
const window = Window("Inside", width, height, scene, fullscreen=false)
const keyboard = Gloria.KeyboardState()

add!(shape_layer, Shape(Circle(0, 0, 100), 0., 0., 0.))
add!(shape_layer, Shape(Polygon(Point(0, 0), Point(100, 0), Point(0, 100)), 200., 0., 40.))
add!(shape_layer, Shape(Lines([Point(100(i - 5), randn()*50 + 200) for i in 0:10]...), 0., 0., -10.))

function main(;keepalive=true)
    Gloria.run!(window)
    if keepalive
        wait(window)
    end
end

end # module
