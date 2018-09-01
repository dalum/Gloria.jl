module Asteroids

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Audio, Event, Layer, Scene, Texture, Window,
    iskey, ispressed, play!

using Colors: @colorant_str

struct Controls <: AbstractObject end

mutable struct Player <: AbstractObject
    texture::Texture
    x::Float64
    y::Float64
    vx::Float64
    vy::Float64
    a::Float64
    θ::Float64
    ω::Float64
end

function onevent!(::Controls, ::Val{:key_down}, e::Event)
    iskey(e, "escape") && Gloria.quit!(window, e)
end

function onevent!(::Player, ::Val{:key_down}, e::Event)
    iskey(e, "space") && play!(laser_sound)
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

function render!(layer::Layer, obj::Player; frame::Int, fps::Float64)
    render!(layer, obj.texture, obj.x, obj.y, angle=-(obj.θ-90))
end

# Setup

# const width, height = 1920, 1080
const width, height = 800, 600
const wrap_width, wrap_height = width + 50, height + 50
const controls_layer = Layer([Controls()], show=false)

const object_layer = Layer(Player[], width/2, height/2, [1. 0.; 0. -1.])

const scene = Scene(controls_layer, object_layer, color=colorant"black")
const window = Window("Asteroids", width, height, scene, fullscreen=false)
const keyboard = Gloria.KeyboardState()

const laser_sound = Audio(abspath(@__DIR__, "..", "assets", "laser.wav"), window)

const player = Player(Texture(window, abspath(@__DIR__, "..", "assets", "player.svg"), width=50, height=50), 0., 0., 0., 0., 100., 0., 180.)
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
