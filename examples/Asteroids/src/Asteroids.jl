module Asteroids

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Audio, Circle, Event, Font, FontRenderer, Layer, Line, Point, Polygon, Resources, Scene, Texture, Window,
    add!, kill!, play!,
    intersects, isalive, iskey, ispressed, text

import Gloria.Physics: interact!
using Gloria.Physics: Physical

using Colors: @colorant_str

##################################################
# Types
##################################################

mutable struct Controls <: AbstractObject
    level::Int
end

abstract type AsteroidsObject <: AbstractObject end

mutable struct Player <: AsteroidsObject
    model::Texture
    a::Float64
    α::Float64
end

mutable struct LaserBeam <: AsteroidsObject
    model::Texture
    t1::Float64
end

mutable struct Rock <: AsteroidsObject
    model::Texture
    scale::Float64
end

function Physical{Player}(a, α)
    model = Texture(resources, abspath(@__DIR__, "..", "assets", "player.svg"), width=50, height=50)
    shape = Polygon(Point(-15., 0.), Point(-25., 15.), Point(25., 0.), Point(-25., -15.))
    return Physical(Player(model, a, α), shape)
end

function Physical{LaserBeam}(x, y, vx, vy, θ)
    model = Texture(resources, abspath(@__DIR__, "..", "assets", "laser.svg"), width=50, height=4)
    shape = Line(-25., 0., 25., 0.)
    shape = Point(0., 0.)
    # shape = Gloria.extrude(shape, 200, 0, 0)
    return Physical(LaserBeam(model, time() + 1), shape; x=x, y=y, θ=θ, vx=vx, vy=vy)
end

function Physical{Rock}(scale, x, y, vx, vy, θ, ω)
    model = Texture(resources, abspath(@__DIR__, "..", "assets", "rock.svg"), width=100, height=100)
    #shape = Circle(0., 0., 50scale)
    n = 12
    shape = Polygon([Point((50cos(2π*i/n)+10randn())*scale, (50sin(2π*i/n)+10randn())*scale) for i in 0:(n-1)]...)
    return Physical(Rock(model, scale), shape, x=x, y=y, θ=θ, vx=vx, vy=vy, ω=ω)
end

##################################################
# Events
##################################################

function onevent!(::Controls, e::Event{:key_down})
    iskey(e, "escape") && Gloria.quit!(window, e)
end

function onevent!(self::Physical{Player}, e::Event{:key_down})
    if iskey(e, "space")
        play!(laser_sound, volume=10)
        add!(object_layer, Physical{LaserBeam}(self.x, self.y, self.vx + cosd(self.θ)*500, self.vy + sind(self.θ)*500, self.θ))
    end
end

function onevent!(self::Physical{Player}, e::Event{:key_up})
    if iskey(e, "right") || iskey(e, "left")
        self.ω = 0.
    end
end

##################################################
# Update
##################################################

function update!(self::Controls; t, dt)
    if count(x->(x isa Physical{Rock} || x isa Physical{LaserBeam}), object_layer) == 0
        self.level += 1
        player.x, player.y = 0., 0.
        player.vx, player.vy = 0., 0.
        startlevel!(self.level)
    end
end

function update!(self::Physical{Player}; t, dt)
    if ispressed(keyboard, "up")
        self.vx += self.wrapped.a*cosd(self.θ)*dt
        self.vy += self.wrapped.a*sind(self.θ)*dt
    end
    if ispressed(keyboard, "left") && abs(self.ω) < 360
        self.ω -= self.wrapped.α*dt
    end
    if ispressed(keyboard, "right") && abs(self.ω) < 360
        self.ω += self.wrapped.α*dt
    end
end

function interact!(self::Physical{Player}, other::Physical{Rock}; t, dt)
    if intersects(self, other, dt)
        kill!(object_layer, other)
        if other.wrapped.scale > 0.25
            for _ in 1:2
                vx = other.vx + (0.5-rand())*50
                vy = other.vy + (0.5-rand())*50
                ω = other.ω + (0.5-rand())*50
                add!(object_layer, Physical{Rock}(other.wrapped.scale/2, other.x, other.y, vx, vy, 360rand(), ω))
            end
        end
        self.x, self.y = 0., 0.
        self.vx, self.vy = 0., 0.
    end
end

function update!(self::Physical{LaserBeam}; t, dt)
    if time() >= self.wrapped.t1
        kill!(object_layer, self)
    end
end

function interact!(self::Physical{LaserBeam}, other::Physical{Rock}; t, dt)
    if isalive(object_layer, self) && intersects(self, other, dt)
        kill!(object_layer, self, other)
        if other.wrapped.scale > 0.25
            for _ in 1:2
                vx = 0.05self.vx/other.scale + other.vx
                vy = 0.05self.vy/other.scale + other.vy
                ω = other.ω + (0.5-rand())*50
                add!(object_layer, Physical{Rock}(other.wrapped.scale/2, other.x, other.y, vx, vy, 360rand(), ω))
            end
        end
    end
end

function Gloria.after_update!(obj::Physical{<:AsteroidsObject}; t, dt)
    obj.x > wrap_width / 2 && (obj.x -= wrap_width)
    obj.x < -wrap_width / 2 && (obj.x += wrap_width)
    obj.y > wrap_height / 2 && (obj.y -= wrap_height)
    obj.y < -wrap_height / 2 && (obj.y += wrap_height)
end

##################################################
# Render
##################################################

function render!(layer::Layer, self::Controls; frame::Int, fps::Float64)
    render!(layer, text(font_noto, "Current level: $(self.level)", color=colorant"#FFFFFF"), 0., 0., 0.)
end

const iswireframe = true

@static if iswireframe
    function render!(layer::Layer, self::Physical{Player}; frame::Int, fps::Float64)
        render!(layer, self.shape, self.x, self.y, self.θ, color=colorant"#C0C0C0")
    end

    function render!(layer::Layer, self::Physical{LaserBeam}; frame::Int, fps::Float64)
        render!(layer, self.shape, self.x, self.y, self.θ, color=colorant"#C0C0C0")
    end

    function render!(layer::Layer, self::Physical{Rock}; frame::Int, fps::Float64)
        render!(layer, self.shape, self.x, self.y, self.θ, color=colorant"#A0A0A0")
    end
else
    function render!(layer::Layer, self::Physical{<:AsteroidsObject}; frame::Int, fps::Float64)
        render!(layer, self.model, self.x, self.y, self.θ)
    end

    function render!(layer::Layer, self::Physical{Rock}; frame::Int, fps::Float64)
        render!(layer, self.model, self.x, self.y, self.θ, scale=self.scale)
    end
end

###

function addrock!(scale, v)
    horv = rand(Bool)
    rock = Physical{Rock}(scale,
                          horv ? rand(-width/2:width/2) : rand([-width/2, width/2]),
                          horv ? rand([-height/2, height/2]) : rand(-height/2:height/2),
                          (0.5-randn())*v, (0.5-randn())*v, 360rand(), 90rand())
    add!(object_layer, rock)
end

function startlevel!(level)
    if level % 3 != 0
        for _ in 1:level
            addrock!(1., 30 + 5level)
        end
    elseif level % 6 != 0
        for _ in 1:3*level
            addrock!(0.25, 30 + 10level)
        end
    else
        for _ in 1:level/3
            addrock!(2.0, 30 + 5level)
        end
    end
end

##################################################
# Setup
##################################################

# const width, height = 1920, 1080
const width, height = 800, 600
const wrap_width, wrap_height = width + 100, height + 100

const controls_layer = Layer([Controls(0)])
const object_layer = Layer(Physical[], width/2, height/2)

const scene = Scene(object_layer, controls_layer, color=colorant"#202020")
const window = Window("Asteroids", width, height, scene, fullscreen=false)

const resources = Resources(window)
const keyboard = Gloria.KeyboardState()

const laser_sound = Audio(resources, abspath(@__DIR__, "..", "assets", "laser.wav"))
const font_noto = FontRenderer(Font(resources, abspath(@__DIR__, "..", "assets", "NotoSans-Black.ttf")),
                               resources)

const player = Physical{Player}(100., 360.)

add!(object_layer, player)

function main(;keepalive=true)
    Gloria.run!(window)
    if keepalive
        wait(window)
    end
end

##################################################
# Precompile
##################################################

# const dir = abspath(@__DIR__, "..", "precompile")
# const blacklist_import = []
# const fnames = collect(filter(x->occursin(r"^precompile_.*\.jl$", x), readdir(dir)))
# const names = (fname->Symbol(match(r"^precompile_(.*)\.jl$", fname)[1])).(fnames)
# for name in names
#     name in blacklist_import && continue
#     try
#         @eval import $name
#     catch e
#         @warn "Failed import of: $name ($e)"
#     end
# end
# for fname in fnames
#     try
#         include(joinpath(dir, fname))
#         _precompile_()
#         catch e
#         @warn "Failed additional precompilation of: $fname ($e)"
#     end
# end

end # module
