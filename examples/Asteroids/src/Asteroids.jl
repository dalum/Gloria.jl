module Asteroids

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Audio, Event, Layer, Scene, Texture, Window,
    iskey, ispressed, play!
using Gloria.Physics: Circle, Line, intersects

using Colors: @colorant_str

struct Controls <: AbstractObject end

abstract type AsteroidsObject <: AbstractObject end

mutable struct Player <: AsteroidsObject
    texture::Texture
    x::Float64
    y::Float64
    vx::Float64
    vy::Float64
    a::Float64
    θ::Float64
    ω::Float64
end

mutable struct LaserBeam <: AsteroidsObject
    texture::Texture
    x::Float64
    y::Float64
    vx::Float64
    vy::Float64
    θ::Float64
    t0::Float64
end
LaserBeam(x, y, vx, vy, θ) = LaserBeam(laser_texture, x, y, vx, vy, θ, time())

function onevent!(::Controls, ::Val{:key_down}, e::Event)
    iskey(e, "escape") && Gloria.quit!(window, e)
end

function onevent!(obj::Player, ::Val{:key_down}, e::Event)
    if iskey(e, "space")
        play!(laser_sound)
        push!(object_layer, LaserBeam(obj.x, obj.y, obj.vx + cos(obj.θ*π/180)*500, obj.vy + sin(obj.θ*π/180)*500, obj.θ))
    end
end

function update!(obj::Player; t::Float64, dt::Float64)
    if ispressed(keyboard, "up")
        obj.vx += obj.a*cos(obj.θ*π/180)*dt
        obj.vy += obj.a*sin(obj.θ*π/180)*dt
    end
    if ispressed(keyboard, "left")
        obj.θ += obj.ω*dt
    end
    if ispressed(keyboard, "right")
        obj.θ -= obj.ω*dt
    end

    obj.x += obj.vx*dt
    obj.y += obj.vy*dt

    obj.x > wrap_width / 2 && (obj.x -= wrap_width)
    obj.x < -wrap_width / 2 && (obj.x += wrap_width)
    obj.y > wrap_height / 2 && (obj.y -= wrap_height)
    obj.y < -wrap_height / 2 && (obj.y += wrap_height)
end

function update!(self::LaserBeam; t::Float64, dt::Float64)
    self.x += self.vx*dt
    self.y += self.vy*dt

    self.x > wrap_width / 2 && (self.x -= wrap_width)
    self.x < -wrap_width / 2 && (self.x += wrap_width)
    self.y > wrap_height / 2 && (self.y -= wrap_height)
    self.y < -wrap_height / 2 && (self.y += wrap_height)

    for other in object_layer
        if other isa LaserBeam && other !== self
            l1 = Line(self.x - 25cos(self.θ*π/180), self.y - 25sin(self.θ*π/180), self.x + 25cos(self.θ*π/180), self.y + 25sin(self.θ*π/180))
            l2 = Line(other.x - 25cos(other.θ*π/180), other.y - 25sin(other.θ*π/180), other.x + 25cos(other.θ*π/180), other.y + 25sin(other.θ*π/180))
            if intersects(l1, l2)
                self.vx = 0
                self.vy = 0
                other.vx = 0
                other.vy = 0
            end
        end
    end
end

function render!(layer::Layer, self::AsteroidsObject; frame::Int, fps::Float64)
    render!(layer, self.texture, self.x, self.y, angle=-(self.θ-90))
end

# Setup

# const width, height = 1920, 1080
const width, height = 800, 600
const wrap_width, wrap_height = width + 50, height + 50
const controls_layer = Layer([Controls()], show=false)

const object_layer = Layer(AbstractObject[], width/2, height/2, [1. 0.; 0. -1.])

const scene = Scene(controls_layer, object_layer, color=colorant"black")
const window = Window("Asteroids", width, height, scene, fullscreen=false)
const keyboard = Gloria.KeyboardState()

const laser_sound = Audio(window, abspath(@__DIR__, "..", "assets", "laser.wav"))
const laser_texture = Texture(window, abspath(@__DIR__, "..", "assets", "laser.svg"), width=4, height=50)

const player_texture = Texture(window, abspath(@__DIR__, "..", "assets", "player.svg"), width=50, height=50)
const player = Player(player_texture, 0., 0., 0., 0., 100., 0., 180.)
push!(object_layer, player)

function main()
    Gloria.run!(window, target_render_speed = 60.0, target_update_speed = 60.0)
    wait(window)
end

# precompile
# const dir = abspath(@__DIR__, "..", "precompile")
# const blacklist_import = [:Asteroids, :unknown]
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
