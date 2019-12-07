woman_module Physics

using Colors
using DataStructures: CircularBuffer, OrderedSet, SortedSet
using LinearAlgebra
using StaticArrays

using Gloria: AbstractObject, AbstractLayer, AbstractScene, RenderTask,
    add!, kill!, populate!
using ..Shapes: Shapes, AbstractShape, BoundingBox, Vertex,
    edges, rotate, trace, translate, vertices

export Physical
    # collide!, timeintersects

import Gloria: after_update!, before_update!, render!, update!, visible
import ..Shapes: inside, intersects

const flatten = Iterators.flatten

const COLLISION_TIME_OFFSET = 1e-5
const MAX_COLLISION_DEPTH = 500

abstract type AbstractPhysicalState{D,R,T} end

mutable struct PhysicalState{D,T,R} <: AbstractPhysicalState{D,T,R}
    t::T
    static::Bool
    resting::Bool
    mass::SMatrix{D,D,T}
    angularmass::SMatrix{R,R,T}
    position::SVector{D,T}
    angle::SVector{R,T}
    velocity::SVector{D,T}
    angularvelocity::SVector{R,T}
    impulse::SVector{D,T}
    angularimpulse::SVector{R,T}

    function PhysicalState{D,T}(
        t::T,
        static::Bool,
        resting::Bool,
        mass::SMatrix{D,D,T},
        angularmass::SMatrix{R,R,T},
        position::SVector{D,T},
        angle::SVector{R,T},
        velocity::SVector{D,T},
        angularvelocity::SVector{R,T},
        impulse::SVector{D,T},
        angularimpulse::SVector{R,T}
    ) where {D,T,R}
        D*(D-1)/2 != R && error("Angular and spatial dimensions do not match: $D*($D-1)/2 != $R")
        return new{D,T,R}(t, static, resting, mass, angularmass, position, angle, velocity, angularvelocity, impulse, angularimpulse)
    end
end
function PhysicalState(t::T, static::Bool, resting::Bool, mass::SMatrix{D,D,T}, angularmass::SMatrix{R,R,T}, position::SVector{D,T}, angle::SVector{R,T}, velocity::SVector{D,T}, angularvelocity::SVector{R,T}) where {D,T,R}
    return PhysicalState{D,T}(t, static, resting, mass, angularmass, position, angle, velocity, angularvelocity, SVector{D}(fill(zero(T), D)), SVector{R}(fill(zero(T), R)))
end

function PhysicalState(t::T, static::Bool, resting::Bool, mass::T, angularmass::T, position::SVector{D,T}, angle::SVector{R,T}, velocity::SVector{D,T}, angularvelocity::SVector{R,T}) where {D,T,R}
    return PhysicalState(t, static, resting, SMatrix{D,D}(Matrix(mass*I, D, D)), SMatrix{R,R}(Matrix(angularmass*I, R, R)), position, angle, velocity, angularvelocity)
end

function PhysicalState(t::T, static::Bool, mass::T, angularmass::T, position::SVector{D,T}, angle::SVector{R,T}, velocity::SVector{D,T}, angularvelocity::SVector{R,T}) where {D,T,R}
    return PhysicalState(t, static, false, SMatrix{D,D}(Matrix(mass*I, D, D)), SMatrix{R,R}(Matrix(angularmass*I, R, R)), position, angle, velocity, angularvelocity)
end

Base.copy(state::PhysicalState{D,T}) where {D,T} = PhysicalState{D,T}(
    state.t,
    state.static,
    state.resting,
    state.mass,
    state.angularmass,
    state.position,
    state.angle,
    state.velocity,
    state.angularvelocity,
    state.impulse,
    state.angularimpulse)

abstract type AbstractPhysicalObject <: AbstractObject end

"""
    Physical{WRAPPED_TYPE, SHAPE_TYPE, PHYSICAL_STATE_TYPE}
"""
mutable struct Physical{T1<:AbstractObject, T2<:AbstractShape, T3<:AbstractPhysicalState} <: AbstractPhysicalObject
    wrapped::T1
    shape::T2
    state_history::CircularBuffer{T3}
end
function Physical(wrapped, shape, state::PhysicalState)
    self = Physical(wrapped, shape, CircularBuffer{PhysicalState}(3))
    fill!(self.state_history, state)
    return self
end
Physical(wrapped, shape, static, mass, angularmass, position, angle, velocity, angularvelocity) =
    Physical(wrapped, shape, PhysicalState(0., static, mass, angularmass, position, angle, velocity, angularvelocity))

function Physical(
    wrapped,
    shape;
    D=2,
    R=floor(Int, D*(D-1)/2),
    static=false,
    mass=1.,
    angularmass=1.,
    position=SVector{D}(fill(0., D)),
    angle=SVector{R}(fill(0., R)),
    velocity=SVector{D}(fill(0., D)),
    angularvelocity=SVector{R}(fill(0., R))
)
    return Physical(wrapped, shape, static, mass, angularmass, position, angle, velocity, angularvelocity)
end

@inline currentstate(obj::Physical) = Core.getfield(obj, :state_history)[end]
@inline previousstate(obj::Physical) = Core.getfield(obj, :state_history)[end-1]
@inline previousstate(obj::Physical, n::Integer) = Core.getfield(obj, :state_history)[end-n]

@inline Base.getproperty(obj::Physical, v::Symbol) = _getproperty(obj, Val(v))
@inline _getproperty(obj::Physical, ::Val{V}) where V = Core.getfield(obj.wrapped, V)
for v in [:(:wrapped), :(:shape), :(:state_history)]
    @eval @inline _getproperty(obj::Physical, ::Val{$v}) = Core.getfield(obj, $v)
end
# for v in [:(:static), :(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
#     @eval @inline _getproperty(obj::Physical, ::Val{$v}) = Core.getfield(currentstate(obj), $v)
# end

@inline Base.setproperty!(obj::Physical, v::Symbol, x) = _setproperty!(obj, Val(v), x)
@inline _setproperty!(obj::Physical, ::Val{V}, x) where V = Core.setfield!(obj.wrapped, V, x)
for v in [:(:wrapped), :(:shape), :(:state_history)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(obj, $v, x)
end
# for v in [:(:static)]
#     @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(currentstate(obj), $v, x)
# end
# for v in [:(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
#     @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(currentstate(obj), $v, convert(Float64, x))
# end

tocoordinates(obj::Physical, p::SVector) = p |> translate(-position(obj)...) |> rotate(-angle(obj)...)

shape(obj::Physical) = obj.shape |> rotate(angle(obj)...) |> translate(position(obj)...)
isstatic(obj::Physical) = currentstate(obj).static

for name in [:mass, :angularmass, :position, :angle, :velocity, :angularvelocity, :impulse, :angularimpulse]
    @eval function $name(obj::Physical)
        return currentstate(obj).$name
    end

    @eval function $(Symbol("set$(name)!"))(obj::Physical, val)
        any(isnan, val) && error("value contains NaN: $val")
        currentstate(obj).$name = val
        return obj
    end
end

function translate!(obj::Physical, val::SVector)
    any(isnan, val) && error("value contains NaN: $val")
    setposition!(obj, position(obj) + val)
end

function rotate!(obj::Physical, val::SVector)
    any(isnan, val) && error("value contains NaN: $val")
    setangle!(obj, angle(obj) + val)
end

function boost!(obj::Physical, val::SVector)
    any(isnan, val) && error("value contains NaN: $val")
    setvelocity!(obj, velocity(obj) + val)
end

function angularboost!(obj::Physical, val::SVector)
    any(isnan, val) && error("value contains NaN: $val")
    setangularvelocity!(obj, angularvelocity(obj) + val)
end

function resetmotion!(obj::Physical)
    setvelocity!(obj, zero(velocity(obj)))
    setangularvelocity!(obj, zero(angularvelocity(obj)))
    return obj
end

function applyimpulse!(obj::Physical, j::SVector, p::SVector)
    r = p - position(obj)
    setimpulse!(obj, impulse(obj) + j)
    setangularimpulse!(obj, angularimpulse(obj) + SVector((r × j)*180/pi))
    return obj
end

function resetimpulse!(obj::Physical)
    setimpulse!(obj, zero(impulse(obj)))
    setangularimpulse!(obj, zero(angularimpulse(obj)))
    return obj
end

momentum(obj::Physical) = mass(obj)*velocity(obj)
angularmomentum(obj::Physical) = angularmass(obj)*angularvelocity(obj)
kineticenergy(obj::Physical) = 0.5*velocity(obj)'mass(obj)*velocity(obj)
angularkineticenergy(obj::Physical) = 0.5*angularvelocity(obj)'angularmass(obj)*angularvelocity(obj)
totalkineticenergy(obj::Physical) = kineticenergy(obj) + angularkineticenergy(obj)

##################################################
# Update
##################################################

function before_update!(obj::Physical, ::AbstractLayer, t::Float64, dt::Float64)
    savestate!(obj, t)
    if !isstatic(obj)
        boost!(obj, impulse(obj)/mass(obj)[1])
        angularboost!(obj, angularimpulse(obj)/angularmass(obj)[1])
        timeevolve!(obj, dt)
    end
    resetimpulse!(obj)
    return obj
end

##################################################
# State functions
##################################################

"""
    savestate!(obj, t)

Save the current state of `obj` and push a copy of it with a new
timestamp, `t`, onto the state history buffer.

"""
function savestate!(obj::Physical, t::Float64)
    push!(obj.state_history, copy(currentstate(obj)))
    currentstate(obj).t = t
    return obj
end

##################################################
# Time translation
##################################################

function timeevolve!(obj::Physical, dt::Float64)
    translate!(obj, dt*velocity(obj))
    rotate!(obj, dt*angularvelocity(obj))
end

function timeinterpolate!(obj::Physical, dt::Float64)
    state = currentstate(obj)
end

##################################################
# Time projection
##################################################

function timecapture(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    shape = shape1 |> rotate(prevstate1.θ) |> translate(prevstate1.x, prevstate1.y)
    lines = unique!(collect(vertices(shape2))) |> rotate(prevstate2.θ) |>
        #!extrude(rotate(state2.θ - prevstate2.θ) |> translate(state2.x - prevstate2.x, state2.y - prevstate2.y)) |>
        translate(prevstate2.x, prevstate2.y)
    return shape, lines
end

function timetrace(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    dt = state1.t - prevstate1.t
    ts = trace(timecapture(shape1, shape2, (state1, prevstate1), (state2, prevstate2))...)
    return (dt * (t - 1.) for t in ts)
end

function timecapture(obj1::Physical, obj2::Physical)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timecapture(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
end

function timetrace(obj1::Physical, obj2::Physical)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timetrace(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
end

timeintersects(obj1::Physical, obj2::Physical) =
    length(timetrace(obj1, obj2)) > 0 || length(timetrace(obj2, obj1)) > 0

##################################################
# Springs
##################################################

# abstract type AbstractHinge{T} <: AbstractPhysicalObject end
# abstract type AbstractSpring{T} <: AbstractPhysicalObject end

# struct Hinge{T} <: AbstractHinge{T}
#     obj::Physical
#     p::Vertex{T}
# end

# struct Spring{T} <: AbstractSpring{T}
#     hinge1::Hinge{T}
#     hinge2::Hinge{T}
#     k::T
#     l0::T
# end

# position(h::AbstractHinge) = h.p |> rotate(h.obj.θ) |> translate(h.obj.x, h.obj.y)
# previousposition(h::AbstractHinge) = h.p |> rotate(previousstate(h.obj).θ) |> translate(previousstate(h.obj).x, previousstate(h.obj).y)
# angle(v::Vertex) = 180*atan(v.y, v.x)/π

intersects(obj1::Physical, obj2::Physical) = intersects(shape(obj1), shape(obj2))
inside(obj::Physical, p::Vertex, θ) = inside(shape(obj), p, θ)

##################################################
# Collisions
##################################################

struct CollisionSolver{D} <: AbstractPhysicalObject
    contact_cache::Dict{Physical,Int}
    displacement_cache::Dict{Physical,SVector{D}}
end
CollisionSolver{D}() where {D} = CollisionSolver{D}(Dict{Physical,Int}(), Dict{Physical,SVector{D}}())

struct Collision{D, T1<:Physical, T2<:Physical, T} <: AbstractPhysicalObject
    t::Float64
    obj1::T1
    obj2::T2
    p::SVector{D,T}
    n::SVector{D,T}
    d::T
end
Base.isless(col1::Collision, col2::Collision) = col1.d < col2.d

function render!(layer::AbstractLayer, self::Collision, frame::Int, fps::Float64)
    render!(layer, Shapes.Polyline(Shapes.Vertex(self.p...), Shapes.Vertex((self.p |> translate((2*self.d)*self.n...))...)), 0., 0., 0., color=colorant"#F00")
    render!(layer, Shapes.Polyline(Shapes.Vertex(self.p...), Shapes.Vertex((self.p |> translate((self.d)*self.n...))...)), 0., 0., 0., color=colorant"#00F")
end

"""
    collisions(layer1, layer2, t, dt)

Return a sorted array of collisions that happened during the last
update step between objects in `layer1` and `layer2`

"""
function collisions(layer::AbstractLayer, t::Float64, dt::Float64)
    # TODO: broad phase / narrow phase separation
    cs = Collision[]
    for (i, obj1) in enumerate(layer.objects), (j, obj2) in enumerate(layer.objects)
        (i <= j || !(obj1 isa Physical) || !(obj2 isa Physical) || (isstatic(obj1) && isstatic(obj2))) && continue
        shape1 = shape(obj1)
        shape2 = shape(obj2)
        !intersects(BoundingBox(shape1), BoundingBox(shape2)) && continue
        d1, n1 = Shapes.separatingaxis(shape1, shape2)
        d2, n2 = Shapes.separatingaxis(shape2, shape1)
        # Shapes are not intersecting
        (d1 < 0 || d2 < 0) && continue
        # Find the shape
        if !(shape2 isa Shapes.HalfSpace) && (d1 <= d2 || shape1 isa Shapes.HalfSpace)
            d, n, p = d1, n1, Shapes.vector(Shapes.supportvertex(shape2, n1))
            push!(cs, Collision(t, obj1, obj2, p, n, d))
        else
            d, n, p = d2, n2, Shapes.vector(Shapes.supportvertex(shape1, n2))
            push!(cs, Collision(t, obj2, obj1, p, n, d))
        end
    end
    return cs
end

function before_update!(self::CollisionSolver, layer::AbstractLayer, t::Float64, dt::Float64)
    for obj in layer
        if obj isa Physical
            self.contact_cache[obj] = get(self.contact_cache, obj, 0)
            self.displacement_cache[obj] = get(self.displacement_cache, obj, zero(position(obj)))
        end
    end
end

function after_update!(self::CollisionSolver, layer::AbstractLayer, t::Float64, dt::Float64)
    cs = collisions(layer, t, dt)

    # Solve positions
    for c in cs
        self.contact_cache[c.obj1] += 1
        self.contact_cache[c.obj2] += 1
    end

    for _ in 1:10
        for c in cs
            solveposition!(c, self)
        end
    end

    for obj in keys(self.displacement_cache)
        self.contact_cache[obj] = 0
        translate!(obj, self.displacement_cache[obj])
        self.displacement_cache[obj] = zero(self.displacement_cache[obj])
    end

    # Solve collisions iteratively
    for _ in 1:1
        for c in cs
            solvevelocity!(c, self)
        end
    end
end

function solveposition!(self::Collision, solver::CollisionSolver)
    Δ = self.d*self.n + solver.displacement_cache[self.obj1] - solver.displacement_cache[self.obj2]
    j = (self.n ⋅ Δ)
    f = isstatic(self.obj1) || isstatic(self.obj2) ? 1. : 0.5
    if !isstatic(self.obj1)
        solver.displacement_cache[self.obj1] -= self.n*j*f/solver.contact_cache[self.obj1]
    end
    if !isstatic(self.obj2)
        solver.displacement_cache[self.obj2] += self.n*j*f/solver.contact_cache[self.obj2]
    end
end

function solvevelocity!(self::Collision, solver::CollisionSolver)
    vr, j, Δ = collide(self.obj1, self.obj2, self.n, self.p, self.d)
    vn = vr ⋅ self.n
    if vn <= 0
        if !isstatic(self.obj1)
            # x = Δ/mass(self.obj1)[1]
            # translate!(self.obj1, -x)
            #x = solver.displacement_cache[self.obj1]
            applyimpulse!(self.obj1, -j, self.p)
            # if norm(impulse(self.obj1)) < 30
            #     setimpulse!(self.obj1, zero(impulse(self.obj1)))
            #     setvelocity!(self.obj1, zero(velocity(self.obj1)))
            # end
        end
        if !isstatic(self.obj2)
            # x = Δ/mass(self.obj2)[1]
            # translate!(self.obj2, x)
            #x = solver.displacement_cache[self.obj2]
            applyimpulse!(self.obj2, j, self.p)
            # if norm(impulse(self.obj2)) < 30
            #     setimpulse!(self.obj2, zero(impulse(self.obj2)))
            #     setvelocity!(self.obj2, zero(velocity(self.obj2)))
            # end
        end
        # setvelocity!(self.obj1, SVector(0., 0.))
        # setvelocity!(self.obj2, SVector(0., 0.))
        # setangularvelocity!(self.obj1, SVector(0.))
        # setangularvelocity!(self.obj2, SVector(0.))
    end
end

"""
    collide(obj1, obj2, n, p, d, CR)

Return a tuple, `vr, j, Δ`, where `vr` is the relative velocity at the
contact point, `p`, `j` is the resulting impulse to be applied at `p`,
and `Δ` is the mass–weighted displacement for resolving the collision.

"""
function collide(obj1::Physical, obj2::Physical, normal::SVector, p::SVector, d::Real)
    m1, m2 = isstatic(obj1) ? Inf : mass(obj1)[1], isstatic(obj2) ? Inf : mass(obj2)[1]
    I1, I2 = isstatic(obj1) ? Inf : angularmass(obj1)[1], isstatic(obj2) ? Inf : angularmass(obj2)[1]
    p1, p2 = position(obj1), position(obj2)
    θ1, θ2 = angle(obj1), angle(obj2)
    v1 = isinf(m1) ? velocity(obj1) : velocity(obj1) + impulse(obj1)/m1
    v2 = isinf(m2) ? velocity(obj2) : velocity(obj2) + impulse(obj2)/m2
    ω1 = isinf(I1) ? angularvelocity(obj1)[1] : angularvelocity(obj1)[1] + angularimpulse(obj1)[1]/I1
    ω2 = isinf(I2) ? angularvelocity(obj2)[1] : angularvelocity(obj2)[1] + angularimpulse(obj2)[1]/I2

    # Setup
    r1 = p - p1
    r2 = p - p2
    vr = (v2 + ω2*(r2 |> rotate(90))*pi/180) - (v1 + ω1*(r1 |> rotate(90))*pi/180)
    vn = vr ⋅ normal

    vperp = vr - vn*normal
    N = norm(vperp)
    tangent = iszero(N) ? vperp : vperp / N
    vt = vr ⋅ tangent

    A1 = r1 × normal
    A2 = r2 × normal
    m = inv(inv(m1) + inv(m2) + inv(I1)*A1^2 + inv(I2)*A2^2)

    # Collision impulse
    R = restitution(obj1, obj2)
    jc = -(1 + R)*vn*m
    if vn^2 < 1000
        jc = max(0., -vn*m)
    end

    # Friction impulse
    F = norm(vr) > 0 ? dynamicfriction(obj1, obj2) : staticfriction(obj1, obj2)
    jf = min(F*jc, vt*m)

    # Total impulse
    j = jc*normal - jf*tangent

    # Displacement share
    Δ = inv(inv(m1) + inv(m2))*normal*d

    return vr, j, Δ
end

restitution(::Physical, ::Physical) = 0.
dynamicfriction(::Physical, ::Physical) = 0.
staticfriction(::Physical, ::Physical) = 0.

end #module
