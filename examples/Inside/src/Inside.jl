module Inside

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Audio, add!, kill!, play!, Scene, Texture,
    Window, isalive, isbutton, iskey, ispressed, getmousestate, tocoordinates,
    Event, Layer
using Gloria.Shapes: AbstractShape, circle, Polyline, Vertex, vertices,
    Polygon, intersects, inside, translate, rotate, centroid

using Colors: RGB, @colorant_str
using LinearAlgebra


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
    if isbutton(e, "left")
        # moved = self.shape |> rotate(self.θ) |> translate(self.x, self.y)
        # vs = vertices(moved)
        # xmin, xmax = extrema(v[1] for v in vs)
        # ymin, ymax = extrema(v[2] for v in vs)
        # c = centroid(moved)
        # mp = Vertex(tocoordinates(shape_layer, e.x, e.y)...)
        # v = normalize(c .- mp)
        # p2 = Vertex((mp .+ 1e6 * v)...)
        # @info "---------------------------"
        # @info typeof(self.shape)
        # @info "Shape center: $c"
        # @info "Boundary: [$xmin, $ymin] x [$xmax, $ymax]"
        # @info "Mouse position: $mp"
        # @info "Direction: $v"
        # @info "Ray target: $p2"
        # @info "---------------------------"
        if inside(
                self.shape |> rotate(self.θ) |> translate(self.x, self.y),
                Vertex(tocoordinates(shape_layer, e.x, e.y)...)
            )
            self.active = true
        end
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

function update!(self::Shape, ::Gloria.AbstractLayer, t, dt)
    if any(
            shape -> any(
                v -> inside(
                    self.shape |> rotate(self.θ) |> translate(self.x, self.y), v
                ),
                vertices(
                    shape.shape |> rotate(shape.θ) |> translate(shape.x, shape.y)
                )
            ),
            filter(x->x ≢ self, shape_layer)
        )

        self.color = colorant"#FA00FA"
    else
        self.color = colorant"#303030"
    end
end

##################################################
# Render
##################################################

function render!(layer::Layer, self::Shape, frame::Int, fps::Float64)
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

const scene = Scene(shape_layer, controls_layer, color=colorant"#D0D0D0")
const window = Window("Inside", width, height, scene, fullscreen=false)
const keyboard = Gloria.KeyboardState()

add!(shape_layer, Shape(circle(Vertex(0., 0.), 100), 0., 0., 0.))
add!(shape_layer, Shape(Polygon(Vertex(0, 0), Vertex(100, 0), Vertex(0, 100)), 200., 0., 40.))
add!(shape_layer, Shape(Polyline([Vertex(100(i - 5), randn()*50 + 200) for i in 0:10]...), 0., 0., -10.))

function main(; keepalive=true)
    Gloria.run!(window)
    keepalive && wait(window)
end

end # module
