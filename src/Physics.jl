module Physics

using DataStructures: CircularBuffer
using Gloria: AbstractObject, Layer
using ..Shapes: AbstractShape, Line, NonPrimitiveShape, Point, Windowing,
    extrude, rotate, trace, transform, translate, vertices

export Gravity, Physical, PhysicalObject,
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

struct Gravity <: PhysicalObject
    x::Float64
    y::Float64
end
Gravity(g; θ) = Gravity(g*cosd(θ), g*sind(θ))

isstatic(::Gravity) = true

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

function update!(obj::PhysicalObject, g::Gravity, t::Float64, dt::Float64)
    if !isstatic(obj)
        accelerate!(obj, dt*g.x, dt*g.y)
    end
    return obj
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

function timeinterpolate(obj::PhysicalObject, dt::Float64)
    dt > 0 && error("Cannot interpolate forward in time (yet).")
    state = currentstate(obj)
    local prevstate
    for prevstate in obj.state_history[end-1:-1:1]
        prevstate.t - state.t < dt && break
        state = prevstate
    end
    
end

##################################################
# Collision
##################################################

intersects(obj1::PhysicalObject, obj2::PhysicalObject) = intersects(
    transform(obj1.shape, rotate(obj1.θ), translate(obj1.x, obj1.y)),
    transform(obj2.shape, rotate(obj2.θ), translate(obj2.x, obj2.y)))

function timetrace(obj1::PhysicalObject, obj2::PhysicalObject)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    Δx = prevstate2.x - prevstate1.x
    Δy = prevstate2.y - prevstate1.y
    δx1 = state1.x - prevstate1.x
    δy1 = state1.y - prevstate1.y
    δθ1 = state1.θ - prevstate1.θ

    δx2 = state2.x - prevstate2.x
    δy2 = state2.y - prevstate2.y
    δθ2 = state2.θ - prevstate2.θ

    shape = rotate(obj1.shape, prevstate1.θ)
    lines = vertices(obj2.shape) |> rotate(prevstate2.θ) |>
        extrude(rotate(δθ2), translate(δx2, δy2), translate(-δx1, δy1), translate(Δx, Δy), rotate(-δθ1), translate(-Δx, -Δy)) |>
        translate(Δx, Δy)

    return trace(shape, lines)
end

function timetrace(obj1::PhysicalObject, obj2::PhysicalObject, dt::Float64)
    
end

timeintersects(obj1::PhysicalObject, obj2::PhysicalObject) =
    length(timetrace(obj1, obj2)) > 0 || length(timetrace(obj2, obj1)) > 0

timeintersects(obj1::Physical{<:Any,<:Point}, obj2::PhysicalObject) =
    length(timetrace(obj2, obj1)) > 0

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

# """
#     (obj1, obj2, dt)

# Return a 

# """
# function (obj1::Physical{<:AbstractObject,<:Shapes.AbstractShape{T}}, obj2::Physical) where T
#     Δx, Δy = obj1.x - obj2.x, obj1.y - obj2.y
#     vxdt = -(obj1.vx - obj2.vx + Δy*obj2.ω*π/180)*dt
#     vydt = -(obj1.vy - obj2.vy - Δx*obj2.ω*π/180)*dt
#     #points = 
#     lines = Vector{Lines{T}}(undef)
#     for point in points(shape1)
#         shape1 = transform(extrude(transform(obj1.shape, 0., 0., obj1.θ), vxdt, vydt, -obj1.ω*dt), Δx, Δy, 0.)
#     end
#     shape2 = transform(obj2.shape, 0, 0, obj2.θ)
#     return shape1, shape2
# end

# """
#     collisiontime(shape1, shape2)

# Return the points where `shape1` intersects `shape2`.

# """
# function collisiontime(obj1::Physical{<:AbstractObject,<:Point}, obj2::Physical{<:AbstractObject,<:Line})

#     t1 = (l1.x1 - l2.x1)*(l2.y1 - l2.y2) - (l1.y1 - l2.y1)*(l2.x1 - l2.x2)
#     t2 = (l1.x1 - l2.x1)*(l1.y1 - l1.y2) - (l1.y1 - l2.y1)*(l1.x1 - l1.x2)
#     d = (l1.x1 - l1.x2)*(l2.y1 - l2.y2) - (l1.y1 - l1.y2)*(l2.x1 - l2.x2)

#     # For parallel lines, `d == 0`.  In this case, either the lines do not touch, or one of the following is true:
#     # - one point from either line intersects with the other, or;
#     # - two points from one line intersects with the other line.
#     # In either case, the intersection is a line segment between these two points, and we return that.
#     if iszero(d)
#         return intersectcoeff(l1, Point(l2.x1, l2.y1)) || intersectcoeff(l1, Point(l2.x2, l2.y2)) ||
#             intersectcoeff(l2, Point(l1.x1, l1.y1)) || intersectcoeff(l2, Point(l1.x2, l1.y2))
#     end

#     return t1/d, t2/d
# end

"""
    collide!(obj1, obj2, nx, ny, x, y, dt; CR=1.0)

Change the position and velocity of `obj1` and `obj2`, given that a
collision happened at the point `x, y`, a time `dt` earlier.  `CR` is
the coefficient of restitution.  It is `1` for a perfectly elastic
collision and 0 for a perfectly inelastic collision.

"""
function collide!(obj1::PhysicalObject, obj2::PhysicalObject, nx, ny, x, y, dt; CR=1.0)
    # See, e. g:
    # https://en.wikipedia.org/wiki/Elastic_collision#Two-dimensional_collision_with_two_moving_objects
    # https://en.wikipedia.org/wiki/Inelastic_collision
    # for details of the two-body collision

    # Step back to the time of collision
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
