module Asteroids

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Audio, Circle, Event, Layer, Line, PhysicalObject, Scene, Texture, Window,
    accelerate!, add!, kill!, play!,
    intersects, iskey, ispressed

using Colors: @colorant_str

struct Controls <: AbstractObject end

abstract type AsteroidsObject <: AbstractObject end

mutable struct Player <: AsteroidsObject
    phy::PhysicalObject
    a::Float64
    α::Float64
end
function Player(a, α)
    shape = Circle(0., 0., 25.)
    texture = Texture(window, abspath(@__DIR__, "..", "assets", "player.svg"), width=50, height=50)
    phy = PhysicalObject(texture, shape)
    Player(phy, a, α)
end

mutable struct LaserBeam <: AsteroidsObject
    phy::PhysicalObject
    t1::Float64
end
function LaserBeam(x, y, vx, vy, θ)
    shape = Line(-25., 0., 25., 0.)
    texture = Texture(window, abspath(@__DIR__, "..", "assets", "laser.svg"), width=50, height=4)
    phy = PhysicalObject(texture, shape, x, y, θ, vx, vy)
    LaserBeam(phy, time() + 1)
end

mutable struct Rock <: AsteroidsObject
    phy::PhysicalObject
    scale::Float64
end
function Rock(scale, x, y, vx, vy, θ, ω)
    shape = Circle(0., 0., 50scale)
    texture = Texture(window, abspath(@__DIR__, "..", "assets", "rock.svg"), width=100, height=100)
    phy = PhysicalObject(texture, shape, x, y, θ, vx, vy, ω)
    Rock(phy, scale)
end

function onevent!(::Controls, ::Val{:key_down}, e::Event)
    iskey(e, "escape") && Gloria.quit!(window, e)
end

function onevent!(self::Player, ::Val{:key_down}, e::Event)
    if iskey(e, "space")
        play!(laser_sound, volume=10)
        add!(object_layer, LaserBeam(self.phy.x, self.phy.y, self.phy.vx + cosd(self.phy.θ)*500, self.phy.vy + sind(self.phy.θ)*500, self.phy.θ))
    end
end

function onevent!(self::Player, ::Val{:key_up}, e::Event)
    if iskey(e, "right") || iskey(e, "left")
        self.phy.ω = 0.
    end
end

function wraparound!(obj::AsteroidsObject)
    obj.phy.x > wrap_width / 2 && (obj.phy.x -= wrap_width)
    obj.phy.x < -wrap_width / 2 && (obj.phy.x += wrap_width)
    obj.phy.y > wrap_height / 2 && (obj.phy.y -= wrap_height)
    obj.phy.y < -wrap_height / 2 && (obj.phy.y += wrap_height)
    return obj
end

function update!(self::Player; t::Float64, dt::Float64)
    if ispressed(keyboard, "up")
        accelerate!(self.phy, self.a*cosd(self.phy.θ), self.a*sind(self.phy.θ), dt=dt)
    end
    if ispressed(keyboard, "left") && abs(self.phy.ω) < 360
        accelerate!(self.phy, 0., 0., -self.α, dt=dt)
    end
    if ispressed(keyboard, "right") && abs(self.phy.ω) < 360
        accelerate!(self.phy, 0., 0., self.α, dt=dt)
    end
    update!(self.phy, dt=dt)
    wraparound!(self)
end

function update!(self::LaserBeam; t::Float64, dt::Float64)
    if time() >= self.t1
        kill!(object_layer, self)
    end
    update!(self.phy, dt=dt)
    wraparound!(self)

    for other in object_layer
        if other isa Rock && intersects(self.phy, other.phy)
            kill!(object_layer, self, other)
            if other.scale > 0.25
                for _ in 1:2
                    vx = 0.2self.phy.vx + other.phy.vx + (0.5-rand())*50
                    vy = 0.2self.phy.vy + other.phy.vy + (0.5-rand())*50
                    ω = other.phy.ω + (0.5-rand())*50
                    add!(object_layer, Rock(other.scale / 2, other.phy.x, other.phy.y, vx, vy, 360rand(), ω))
                end
            end
        end
    end
end

function update!(self::Rock; t::Float64, dt::Float64)
    update!(self.phy, dt=dt)
    wraparound!(self)
end

function render!(layer::Layer, self::AsteroidsObject; frame::Int, fps::Float64)
    render!(layer, self.phy)
end

function render!(layer::Layer, self::Rock; frame::Int, fps::Float64)
    render!(layer, self.phy, scale=self.scale)
end

# Setup

# const width, height = 1920, 1080
const width, height = 800, 600
const wrap_width, wrap_height = width + 100, height + 100
const controls_layer = Layer([Controls()], show=false)

const object_layer = Layer(AbstractObject[], width/2, height/2)

const scene = Scene(controls_layer, object_layer, color=colorant"black")
const window = Window("Asteroids", width, height, scene, fullscreen=false)
const keyboard = Gloria.KeyboardState()

const laser_sound = Audio(window, abspath(@__DIR__, "..", "assets", "laser.wav"))

const player = Player(100., 360.)

add!(object_layer, player, Rock(1., rand(-width/2:width/2), rand(-height/2:height/2), (0.5-randn())*50., (0.5-randn())*50., 360rand(), 360rand()))

function main()
    Gloria.run!(window)
    wait(window)
end

# precompile
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
