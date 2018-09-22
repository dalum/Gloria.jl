module Physics

using DataStructures: CircularBuffer, SortedSet
using Gloria: AbstractObject, Layer,
    populate!
using ..Shapes: AbstractShape, Line, NonPrimitiveShape, Point,
    extrude, rotate, trace, translate, vertices

export Physical,
    collide!, oncollision!, timeintersects

import Gloria: after_update!, before_update!, render!, update!

const COLLISION_TIME_OFFSET = 1e-8

abstract type AbstractPhysicalState end

mutable struct PhysicalState <: AbstractPhysicalState
    t::Float64
    static::Bool
    m::Float64
    I::Float64
    x::Float64
    y::Float64
    θ::Float64
    vx::Float64
    vy::Float64
    ω::Float64
end

Base.copy(state::PhysicalState) = PhysicalState(
    state.t,
    state.static,
    state.m,
    state.I,
    state.x,
    state.y,
    state.θ,
    state.vx,
    state.vy,
    state.ω)

"""

"""
mutable struct Physical{T1 <: AbstractObject, T2 <: AbstractShape, T3 <: AbstractPhysicalState} <: AbstractObject
    wrapped::T1
    shape::T2
    state_history::CircularBuffer{T3}
end
function Physical(wrapped, shape, state::PhysicalState)
    self = Physical(wrapped, shape, CircularBuffer{PhysicalState}(3))
    fill!(self.state_history, state)
    return self
end
Physical(wrapped, shape, static, m, I, x, y, θ, vx, vy, ω) =
    Physical(wrapped, shape, PhysicalState(0., static, convert.(Float64, (m, I, x, y, θ, vx, vy, ω))...))
Physical(wrapped, shape; static=false, m=1., I=1., x=0., y=0., θ=0., vx=0., vy=0., ω=0.) =
    Physical(wrapped, shape, static, m, I, x, y, θ, vx, vy, ω)

@inline currentstate(obj::Physical) = Core.getfield(obj, :state_history)[end]
@inline previousstate(obj::Physical) = Core.getfield(obj, :state_history)[end-1]

@inline Base.getproperty(obj::Physical, v::Symbol) = _getproperty(obj, Val(v))
@inline _getproperty(obj::Physical, ::Val{V}) where V = Core.getfield(obj.wrapped, V)
for v in [:(:wrapped), :(:shape), :(:state_history)]
    @eval @inline _getproperty(obj::Physical, ::Val{$v}) = Core.getfield(obj, $v)
end
for v in [:(:static), :(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
    @eval @inline _getproperty(obj::Physical, ::Val{$v}) = Core.getfield(currentstate(obj), $v)
end

@inline Base.setproperty!(obj::Physical, v::Symbol, x) = _setproperty!(obj, Val(v), x)
@inline _setproperty!(obj::Physical, ::Val{V}, x) where V = Core.setfield!(obj.wrapped, V, x)
for v in [:(:wrapped), :(:shape), :(:state_history)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(obj, $v, x)
end
for v in [:(:static)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(currentstate(obj), $v, x)
end
for v in [:(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(currentstate(obj), $v, convert(Float64, x))
end

Base.size(obj::Physical) =sqrt(maximum(p->p.x^2+p.y^2, vertices(obj.shape)))

wrapped(obj::Physical) = obj.wrapped
isstatic(obj::Physical) = obj.static

mass(obj::Physical) = obj.m
position(obj::Physical) = (obj.x, obj.y)
velocity(obj::Physical) = (obj.vx, obj.vy)
angularmass(obj::Physical) = obj.I
angle(obj::Physical) = obj.θ
angularvelocity(obj::Physical) = obj.ω

setmass!(obj::Physical, m) = (obj.m = m; obj)
setposition!(obj::Physical, x, y) = (obj.x = x; obj.y = y; obj)
setvelocity!(obj::Physical, vx, vy) = (obj.vx = vx; obj.vy = vy; obj)
setangularmass!(obj::Physical, I) = (obj.I = I; obj)
setangle!(obj::Physical, θ) = (obj.θ = θ; obj)
setangularvelocity!(obj::Physical, ω) = (obj.ω = ω; obj)

@inline function translate!(obj::Physical, x′, y′)
    x, y = position(obj)
    setposition!(obj, x + x′, y + y′)
end

rotate!(obj::Physical, θ′) = setangle!(obj, angle(obj) + θ′)

momentum(obj::Physical) = mass(obj).*velocity(obj)
angularmomentum(obj::Physical) = angularmass(obj)*angularvelocity(obj)
kineticenergy(obj::Physical) = 0.5mass(obj)*norm(velocity(obj))^2
angularkineticenergy(obj::Physical) = 0.5angularmass(obj)*angularvelocity(obj)^2
totalkineticenergy(obj::Physical) = kineticenergy(obj) + angularkineticenergy(obj)

##################################################
# Update
##################################################

function oncollision!(::Physical, ::Physical, t::Float64, dt::Float64) end

function before_update!(layer::Layer{Physical}, t::Float64, dt::Float64)
    populate!(layer)
    for obj in layer.objects
        before_update!(obj, t, dt)
    end
    while (cs = collisions(layer, t, dt); length(cs) > 0)
        c = first(cs)
        δt = t - c.t

        for obj in layer.objects
            timeevolve!(obj, -δt)
            currentstate(obj).t = c.t
        end
        collide!(c.obj1, c.obj2, 0., 1., 0., 0.)
        for obj in layer.objects
            savestate!(obj, c.t)
            timeevolve!(obj, δt)
            currentstate(obj).t = t
        end
    end
    return layer
end

function before_update!(obj::Physical, t::Float64, dt::Float64)
    savestate!(obj, t)
    if !isstatic(obj)
        timeevolve!(obj, dt)
    end
    return obj
end

##################################################
# Time translation
##################################################

function savestate!(obj::Physical, t::Float64)
    push!(obj.state_history, copy(currentstate(obj)))
    currentstate(obj).t = t
    return obj
end

function timeevolve!(obj::Physical, dt::Float64)
    state = currentstate(obj)
    vx = state.vx
    vy = state.vy
    ω = state.ω
    translate!(obj, dt*vx, dt*vy)
    rotate!(obj, dt*ω)
end

function timeinterpolate(obj::Physical, dt::Float64)
    dt > 0 && error("Cannot interpolate forward in time (yet).")
    state = currentstate(obj)
    t = state.t
    local prevstate
    for prevstate in obj.state_history[end-1:-1:1]
        prevstate.t - t < dt && break
        state = prevstate
    end
    prevstate.t - state.t
end

##################################################
# Collision
##################################################

intersects(obj1::Physical, obj2::Physical) = intersects(
    obj1.shape |> rotate(obj1.θ) |> translate(obj1.x, obj1.y),
    obj2.shape |> rotate(obj2.θ) |> translate(obj2.x, obj2.y))

function timecapture(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    Δx = prevstate2.x - prevstate1.x
    Δy = prevstate2.y - prevstate1.y

    δx1 = state1.x - prevstate1.x
    δy1 = state1.y - prevstate1.y
    δθ1 = state1.θ - prevstate1.θ
    δx2 = state2.x - prevstate2.x
    δy2 = state2.y - prevstate2.y
    δθ2 = state2.θ - prevstate2.θ

    shape = rotate(shape1, prevstate1.θ)
    lines = collect(vertices(shape2)) |> rotate(prevstate2.θ) |>
        extrude(rotate(δθ2) |> translate(δx2, δy2) |> translate(-δx1, -δy1) |> translate(Δx, Δy) |> rotate(-δθ1) |> translate(-Δx, -Δy)) |>
        translate(Δx, Δy)
    return shape, lines
end

function timecapture(obj1::Physical, obj2::Physical)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timecapture(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
end


function timetrace(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    dt = state1.t - prevstate1.t
    ts = trace(timecapture(shape1, shape2, (state1, prevstate1), (state2, prevstate2))...)
    return dt .* (ts .- 1.)
end

function timetrace(obj1::Physical, obj2::Physical)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timetrace(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
end

timeintersects(obj1::Physical, obj2::Physical) =
    length(timetrace(obj1, obj2)) > 0 || length(timetrace(obj2, obj1)) > 0

##################################################
# Collisions
##################################################

struct Collision{T1 <: Physical, T2 <: Physical}
    obj1::T1
    obj2::T2
    t::Float64
end
Base.:(==)(col1::Collision, col2::Collision) = col1.t == col2.t && ((col1.obj1 == col2.obj1 && col1.obj2 == col2.obj2) || (col1.obj1 == col2.obj2 && col1.obj2 == col2.obj1))
Base.isless(col1::Collision, col2::Collision) = col1.t < col2.t

"""
    collisions(layer)

Return a sorted array of collisions that happened in the last update
step.

"""
function collisions(layer::Layer{<:Physical}, t::Float64, dt::Float64)
    cs = SortedSet{Collision}()
    for i in 1:length(layer.objects), j in i+1:length(layer.objects)
        ct = collisiontime(layer.objects[i], layer.objects[j])
        if ct !== nothing && ct + dt > COLLISION_TIME_OFFSET
            push!(cs, Collision(layer.objects[i], layer.objects[j], t + ct - COLLISION_TIME_OFFSET))
        end
    end
    return cs
end

"""
    collisiontime(shape1, shape2)

Return the time `dt` into past to find the earliest point where
`shape1` intersects `shape2`.

"""
function collisiontime(obj1::Physical, obj2::Physical)
    ts = union(timetrace(obj1, obj2), timetrace(obj2, obj1))
    if length(ts) > 0
        return minimum(ts)
    else
        return nothing
    end
end

"""
    collide!(obj1, obj2, nx, ny, x, y, dt; CR=1.0)

Change the position and velocity of `obj1` and `obj2`, given that a
collision happened at the point `x, y`, a time `dt` earlier.  `CR` is
the coefficient of restitution.  It is `1` for a perfectly elastic
collision and 0 for a perfectly inelastic collision.

"""
function collide!(obj1::Physical, obj2::Physical, nx, ny, x, y; CR=1.0)
    # See, e. g:
    # https://en.wikipedia.org/wiki/Elastic_collision#Two-dimensional_collision_with_two_moving_objects
    # https://en.wikipedia.org/wiki/Inelastic_collision
    # for details of the two-body collision

    # Get properties
    m1, m2 = isstatic(obj1) ? Inf : mass(obj1), isstatic(obj2) ? Inf : mass(obj2)
    (x1, y1), (x2, y2) = position(obj1), position(obj2)
    (vx1, vy1), (vx2, vy2) = velocity(obj1), velocity(obj2)

    Δx = x1 - x2
    Δy = y1 - y2
    Δvx = vx1 - vx2
    Δvy = vy1 - vy2

    f1 = isinf(m2) ? 1.0 : m2 / (m1 + m2)
    f2 = isinf(m1) ? 1.0 : m1 / (m1 + m2)

    C1 = -(1 + CR) * f1 * (Δvx*nx + Δvy*ny)
    C2 = (1 + CR) * f2 * (Δvx*nx + Δvy*ny)

    obj1.vx += C1 * nx
    obj1.vy += C1 * ny
    obj2.vx += C2 * nx
    obj2.vy += C2 * ny

    return obj1
end

end #module
