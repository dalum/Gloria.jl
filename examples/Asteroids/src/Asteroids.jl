module Asteroids

import Gloria: onevent!, render!, update!
using Gloria: Gloria, AbstractObject, Audio, Circle, Event, Font, Layer, Line, Point, Polygon, Resources, Scene, Texture, Window,
    add!, kill!, play!,
    intersects, isalive, iskey, ispressed, text

using Gloria.Physics: Physical

using Colors: @colorant_str

##################################################
# Types
##################################################

mutable struct Controls <: AbstractObject
    level::Int
    lives::Int
    transition::Bool
    t1::Float64
end
Controls() = Controls(0, 3, false, 0.)

struct Banner <: AbstractObject
    text::String
    t1::Float64
end

abstract type AsteroidsObject <: AbstractObject end

mutable struct Player <: AsteroidsObject
    model::Texture
    a::Float64
    α::Float64
end

mutable struct Shield <: AsteroidsObject
    t1::Float64
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

function Physical{Shield}(t1)
    shape = Circle(0., 0., 50)
    return Physical(Shield(t1), shape)
end

function Physical{LaserBeam}(t, x, y, vx, vy, θ)
    model = Texture(resources, abspath(@__DIR__, "..", "assets", "laser.svg"), width=50, height=4)
    shape = Line(-25., 0., 25., 0.)
    shape = Point(0., 0.)
    # shape = Gloria.extrude(shape, 200, 0, 0)
    return Physical(LaserBeam(model, t + 1), shape; x=x, y=y, θ=θ, vx=vx, vy=vy)
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

function onevent!(self::Controls, e::Event{:key_down}, t, dt)
    self.level == 0 && iskey(e, "space") && nextlevel!(t)
end

function onevent!(self::Physical{Player}, e::Event{:key_down}, t, dt)
    if controls.level > 0 && iskey(e, "space")
        play!(laser_sound, volume=10)
        add!(object_layer, Physical{LaserBeam}(t, self.x, self.y, self.vx + cosd(self.θ)*500, self.vy + sind(self.θ)*500, self.θ))
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

function update!(self::Controls, t, dt)
    if self.level > 0 && count(x->x isa Physical{Rock}, object_layer) == 0
        if !self.transition
            self.t1 = t + 1
            self.transition = true
        end

        if t > self.t1
            self.transition = false
            nextlevel!(t)
        end
    end
    if self.lives < 0
        gameover!(t)
    end
end

function update!(self::Banner, t, dt)
    if t > self.t1
        kill!(banner_layer, self)
    end
end

function update!(self::Physical{Player}, t, dt)
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

function update!(self::Physical{Player}, other::Physical{Rock}, t, dt)
    if isalive(object_layer, other) && intersects(self, other, dt)
        destroyrock!(other, self)

        if !any(x->x isa Physical{Shield}, object_layer)
            self.x, self.y = 0., 0.
            self.vx, self.vy = 0., 0.
            controls.lives -= 1
            if controls.lives > 0
                add!(object_layer, Physical{Shield}(t + 2))
            end
        end
    end
end

function update!(self::Physical{Shield}, t, dt)
    self.x = player.x
    self.y = player.y
    if t > self.t1
        kill!(object_layer, self)
    end
end

function update!(self::Physical{Shield}, other::Physical{Rock}, t, dt)
    if intersects(self, other, dt)
        destroyrock!(other, self)
    end
end

function update!(self::Physical{LaserBeam}, t, dt)
    if t >= self.wrapped.t1
        kill!(object_layer, self)
    end
end

function update!(self::Physical{LaserBeam}, other::Physical{Rock}, t, dt)
    if isalive(object_layer, self) && intersects(self, other, dt)
        kill!(object_layer, self)
        destroyrock!(other, self)
    end
end

function Gloria.after_update!(obj::Physical{<:AsteroidsObject}, t, dt)
    obj.x > wrap_width / 2 && (obj.x -= wrap_width)
    obj.x < -wrap_width / 2 && (obj.x += wrap_width)
    obj.y > wrap_height / 2 && (obj.y -= wrap_height)
    obj.y < -wrap_height / 2 && (obj.y += wrap_height)
end

##################################################
# Render
##################################################

function render!(layer::Layer, self::Banner, frame::Int, fps::Float64)
    render!(layer, text(font_noto, self.text, color=colorant"#FFFFFF", halign=0.5, valign=0.5), 0., 0., 0.)
end

function render!(layer::Layer, self::Controls, frame::Int, fps::Float64)
    if self.level > 0
        render!(layer, text(font_noto, "Lives: $(self.lives)", color=colorant"#FFFFFF"), 0., 0., 0.)
    else
        render!(layer, text(font_noto, "Welcome to Asteroids!", color=colorant"#FFFFFF", halign=0.5, valign=0.5), width/2, height/2 - 50, 0.)
        if (frame % 50) <= 25
            render!(layer, text(font_noto, "Press space to start", color=colorant"#FFFFFF", halign=0.5, valign=0.5), width/2, height/2 + 50, 0.)
        end
    end
end

const iswireframe = true

function render!(layer::Layer, self::Physical{<:Union{Player,Shield,LaserBeam}}, frame::Int, fps::Float64)
    render!(layer, self.shape, self.x, self.y, self.θ, color=colorant"#C0C0C0")
end

function render!(layer::Layer, self::Physical{Rock}, frame::Int, fps::Float64)
    render!(layer, self.shape, self.x, self.y, self.θ, color=colorant"#A0A0A0")
end

##################################################
# Helper functions
##################################################

function addrock!(scale, v)
    horv = rand(Bool)
    rock = Physical{Rock}(scale,
                          horv ? rand(-width/2:width/2) : rand([-width/2, width/2]),
                          horv ? rand([-height/2, height/2]) : rand(-height/2:height/2),
                          (0.5-randn())*v, (0.5-randn())*v, 360rand(), 90rand())
    add!(object_layer, rock)
end

function destroyrock!(rock, other)
    kill!(object_layer, rock)
    if rock.scale > 0.25
        for _ in 1:2
            vx = 0.1other.vx/rock.scale + rock.vx
            vy = 0.1other.vy/rock.scale + rock.vy
            ω = rock.ω + 50*(rand() - 0.5)
            add!(object_layer, Physical{Rock}(rock.scale/2, rock.x, rock.y, vx, vy, 360rand(), ω))
        end
    end
end

function nextlevel!(t)
    controls.level += 1
    level = controls.level
    player.x, player.y = 0., 0.
    player.vx, player.vy = 0., 0.

    for laserbeam in filter(x->x isa Physical{LaserBeam}, object_layer)
        kill!(object_layer, laserbeam)
    end

    add!(banner_layer, Banner("Level $level", t + 1))
    add!(object_layer, Physical{Shield}(t + 2))
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

function gameover!(t)
    kill!(object_layer, player)
    kill!(controls_layer, controls)
    controls.level = 0
    controls.lives = 3
    add!(banner_layer, Banner("Game over", t + 3))
end

##################################################
# Setup
##################################################

# const width, height = 1920, 1080
const width, height = 800, 600
const wrap_width, wrap_height = width + 100, height + 100

const controls = Controls()
const controls_layer = Layer([controls])
const banner_layer = Layer(Banner[], width/2, height/2)
const object_layer = Layer(Physical[], width/2, height/2)

const scene = Scene(object_layer, banner_layer, controls_layer, color=colorant"#202020")
const window = Window("Asteroids", width, height, scene, fullscreen=false)

const resources = Resources(window)
const keyboard = Gloria.KeyboardState()

const laser_sound = Audio(resources, abspath(@__DIR__, "..", "assets", "laser.wav"))
const font_noto = Font(resources, abspath(@__DIR__, "..", "assets", "NotoSans-Black.ttf"), fontsize=24)

const player = Physical{Player}(100., 360.)

add!(object_layer, player, Physical{Shield}(3))

function main(; keepalive=true)
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
