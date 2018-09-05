module Asteroids

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Audio, Circle, Event, Layer, Line, Scene, Texture, Window,
    add!, kill!, play!,
    intersects, iskey, ispressed

import Gloria.Physics: interact!
using Gloria.Physics: Physical

using Colors: @colorant_str

##################################################
# Types
##################################################

struct Controls <: AbstractObject end

abstract type AsteroidsObject <: AbstractObject end

struct Player <: AsteroidsObject
    model::Texture
    a::Float64
    α::Float64
end

struct LaserBeam <: AsteroidsObject
    model::Texture
    t1::Float64
end

struct Rock <: AsteroidsObject
    model::Texture
    scale::Float64
end

function Physical{Player}(a, α)
    model = Texture(window, abspath(@__DIR__, "..", "assets", "player.svg"), width=50, height=50)
    shape = Circle(0., 0., 25.)
    return Physical(Player(model, a, α), shape)
end

function Physical{LaserBeam}(x, y, vx, vy, θ)
    model = Texture(window, abspath(@__DIR__, "..", "assets", "laser.svg"), width=50, height=4)
    shape = Line(-25., 0., 25., 0.)
    return Physical(LaserBeam(model, time() + 1), shape; x=x, y=y, θ=θ, vx=vx, vy=vy)
end

function Physical{Rock}(scale, x, y, vx, vy, θ, ω)
    model = Texture(window, abspath(@__DIR__, "..", "assets", "rock.svg"), width=100, height=100)
    shape = Circle(0., 0., 50scale)
    return Physical(Rock(model, scale), shape, x=x, y=y, θ=θ, vx=vx, vy=vy, ω=ω)
end

##################################################
# Events
##################################################

function onevent!(::Controls, ::Val{:key_down}, e::Event)
    iskey(e, "escape") && Gloria.quit!(window, e)
end

function onevent!(self::Physical{Player}, ::Val{:key_down}, e::Event)
    if iskey(e, "space")
        play!(laser_sound, volume=10)
        add!(object_layer, Physical{LaserBeam}(self.x, self.y, self.vx + cosd(self.θ)*500, self.vy + sind(self.θ)*500, self.θ))
    end
end

function onevent!(self::Physical{Player}, ::Val{:key_up}, e::Event)
    if iskey(e, "right") || iskey(e, "left")
        self.ω = 0.
    end
end

##################################################
# Update
##################################################

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

function update!(self::Physical{LaserBeam}; t, dt)
    if time() >= self.wrapped.t1
        kill!(object_layer, self)
    end
end

function interact!(self::Physical{LaserBeam}, other::Physical{Rock}; t, dt)
    if intersects(self, other)
        kill!(object_layer, self, other)
        if other.wrapped.scale > 0.25
            for _ in 1:2
                vx = 0.2self.vx + other.vx + (0.5-rand())*50
                vy = 0.2self.vy + other.vy + (0.5-rand())*50
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

function render!(layer::Layer, self::AsteroidsObject, x, y, θ; frame::Int, fps::Float64)
    render!(layer, self.model, x, y, θ)
end

function render!(layer::Layer, self::Rock, x, y, θ; frame::Int, fps::Float64)
    render!(layer, self.model, x, y, θ, scale=self.scale)
end

##################################################
# Setup
##################################################

# const width, height = 1920, 1080
const width, height = 800, 600
const wrap_width, wrap_height = width + 100, height + 100
const controls_layer = Layer([Controls()], show=false)

const object_layer = Layer(Physical[], width/2, height/2)

const scene = Scene(controls_layer, object_layer, color=colorant"black")
const window = Window("Asteroids", width, height, scene, fullscreen=false)
const keyboard = Gloria.KeyboardState()

const laser_sound = Audio(window, abspath(@__DIR__, "..", "assets", "laser.wav"))

const player = Physical{Player}(100., 360.)

add!(object_layer, player, Physical{Rock}(1., rand(-width/2:width/2), rand(-height/2:height/2), (0.5-randn())*50., (0.5-randn())*50., 360rand(), 360rand()))

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
