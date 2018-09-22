module Physics

using DataStructures: CircularBuffer
using Gloria: AbstractObject, Layer
using ..Shapes: AbstractShape, Line, NonPrimitiveShape, Point, Windowing,
    extrude, rotate, trace, transform, translate, vertices

export Physical, PhysicalObject,
    collide!

abstract type PhysicalObject <: AbstractObject end
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
mutable struct Physical{T1 <: AbstractObject, T2 <: AbstractShape, T3 <: AbstractPhysicalState} <: PhysicalObject
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

@inline function translate!(obj::PhysicalObject, x′, y′)
    x, y = position(obj)
    setposition!(obj, x + x′, y + y′)
end

@inline function accelerate!(obj::PhysicalObject, vx′, vy′)
    vx, vy = velocity(obj)
    setvelocity!(obj, vx + vx′, vy + vy′)
end

rotate!(obj::PhysicalObject, θ′) = setangle!(obj, angle(obj) + θ′)

momentum(obj::PhysicalObject) = mass(obj).*velocity(obj)
angularmomentum(obj::PhysicalObject) = angularmass(obj)*angularvelocity(obj)
kineticenergy(obj::PhysicalObject) = 0.5mass(obj)*norm(velocity(obj))^2
angularkineticenergy(obj::PhysicalObject) = 0.5angularmass(obj)*angularvelocity(obj)^2
totalkineticenergy(obj::PhysicalObject) = kineticenergy(obj) + angularkineticenergy(obj)

##################################################
# Update
##################################################

import Gloria: after_update!, before_update!, render!, update!

function before_update!(obj::PhysicalObject, t::Float64, dt::Float64)
    push!(obj.state_history, copy(currentstate(obj)))
    currentstate(obj).t = t
    if !isstatic(obj)
        timeevolve!(obj, dt)
    end
end

##################################################
# Time translation
##################################################

@inline function timeevolve!(obj::PhysicalObject, dt::Float64)
    vx, vy = velocity(obj)
    ω = angularvelocity(obj)
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

intersects(obj1::PhysicalObject, obj2::PhysicalObject) = intersects(
    transform(obj1.shape, rotate(obj1.θ), translate(obj1.x, obj1.y)),
    transform(obj2.shape, rotate(obj2.θ), translate(obj2.x, obj2.y)))

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
    lines = vertices(shape2) |> rotate(prevstate2.θ) |>
        extrude(rotate(δθ2), translate(δx2, δy2), translate(-δx1, δy1), translate(Δx, Δy), rotate(-δθ1), translate(-Δx, -Δy)) |>
        translate(Δx, Δy)
    return shape, lines
end

function timecapture(obj1::PhysicalObject, obj2::PhysicalObject)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timecapture(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
end


function timetrace(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    dt = state1.t - prevstate1.t
    return dt .* (trace(timecapture(shape1, shape2, (state1, prevstate1), (state2, prevstate2))...) .- 1.)
end

function timetrace(obj1::PhysicalObject, obj2::PhysicalObject)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timetrace(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
end

timeintersects(obj1::PhysicalObject, obj2::PhysicalObject) =
    length(timetrace(obj1, obj2)) > 0 || length(timetrace(obj2, obj1)) > 0

function centerofmass(objs)
    mx = my = M = 0.0
    for obj in objs
        x′, y′ = position(obj)
        m = mass(obj)
        M += m
        mx += m*x′
        my += m*y′
    end
    return mx / M, my / M
end

"""
    collisiontime(shape1, shape2)

Return the time `dt` into past to find the earliest point where
`shape1` intersects `shape2`.

"""
collisiontime(obj1::Physical, obj2::Physical) = minimum(union(timetrace(obj1, obj2), timetrace(obj2, obj1)))

"""
    collide!(obj1, obj2, nx, ny, x, y, dt; CR=1.0)

Change the position and velocity of `obj1` and `obj2`, given that a
collision happened at the point `x, y`, a time `dt` earlier.  `CR` is
the coefficient of restitution.  It is `1` for a perfectly elastic
collision and 0 for a perfectly inelastic collision.

"""
function collide!(obj1::PhysicalObject, obj2::PhysicalObject, nx, ny, x, y; CR=1.0)
    # See, e. g:
    # https://en.wikipedia.org/wiki/Elastic_collision#Two-dimensional_collision_with_two_moving_objects
    # https://en.wikipedia.org/wiki/Inelastic_collision
    # for details of the two-body collision

    # Step back to the time of collision
    dt = collisiontime(obj1, obj2)
    timeevolve!(obj1, -dt)
    timeevolve!(obj2, -dt)

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
    C2 = -(1 + CR) * f2 * (Δvx*nx + Δvy*ny)

    Δvx1′ = C1 * nx
    Δvy1′ = C1 * ny
    Δvx2′ = -C2 * nx
    Δvy2′ = -C2 * ny

    accelerate!(obj1, Δvx1′, Δvy1′)
    accelerate!(obj2, Δvx2′, Δvy2′)

    # Evolve forward to current time
    timeevolve!(obj1, dt)
    timeevolve!(obj2, dt)

    return obj1
end

end #module
