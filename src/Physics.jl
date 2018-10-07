module Physics

using Colors
using DataStructures: CircularBuffer, OrderedSet, SortedSet
using LinearAlgebra
using StaticArrays

using Gloria: AbstractObject, AbstractLayer, AbstractScene, RenderTask,
    add!, kill!, populate!
using ..Shapes: Shapes, AbstractShape, BoundingBox,
    edges, rotate, trace, translate, vertices

export CollisionLayer, Physical
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
    mass::SMatrix{D,D,T}
    angularmass::SMatrix{R,R,T}
    position::SVector{D,T}
    angle::SVector{R,T}
    velocity::SVector{D,T}
    angularvelocity::SVector{R,T}

    function PhysicalState{D,T}(t::T, static::Bool, mass::SMatrix{D,D,T}, angularmass::SMatrix{R,R,T}, position::SVector{D,T}, angle::SVector{R,T}, velocity::SVector{D,T}, angularvelocity::SVector{R,T}) where {D,T,R}
        D*(D-1)/2 != R && error("Angular and spatial dimensions do not match: $D*($D-1)/2 != $R")
        return new{D,T,R}(t, static, mass, angularmass, position, angle, velocity, angularvelocity)
    end
end
function PhysicalState(t::T, static::Bool, mass::SMatrix{D,D,T}, angularmass::SMatrix{R,R,T}, position::SVector{D,T}, angle::SVector{R,T}, velocity::SVector{D,T}, angularvelocity::SVector{R,T}) where {D,T,R}
    return PhysicalState{D,T}(t, static, mass, angularmass, position, angle, velocity, angularvelocity)
end

function PhysicalState(t::T, static::Bool, mass::T, angularmass::T, position::SVector{D,T}, angle::SVector{R,T}, velocity::SVector{D,T}, angularvelocity::SVector{R,T}) where {D,T,R}
    return PhysicalState(t, static, SMatrix{D,D}(Matrix(mass*I, D, D)), SMatrix{R,R}(Matrix(angularmass*I, R, R)), position, angle, velocity, angularvelocity)
end

Base.copy(state::PhysicalState) = PhysicalState(
    state.t,
    state.static,
    state.mass,
    state.angularmass,
    state.position,
    state.angle,
    state.velocity,
    state.angularvelocity)

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

tocoordinates(obj::Physical, p::SVector{2}) = p |> translate(-position(obj)...) |> rotate(-angle(obj)...)

shape(obj::Physical) = obj.shape |> rotate(angle(obj)...) |> translate(position(obj)...)
# wrapped(obj::Physical) = obj.wrapped
isstatic(obj::Physical) = currentstate(obj).static

# mass(obj::Union{Physical}) = currentstate(obj).mass
# angularmass(obj::Union{Physical}) = currentstate(obj).angularmass
# position(obj::Union{Physical}) = currentstate(obj).position
# angle(obj::Union{Physical}) = currentstate(obj).angle
# velocity(obj::Union{Physical}) = currentstate(obj).velocity
# angularvelocity(obj::Union{Physical}) = currentstate(obj).angularvelocity

# setmass!(obj::Union{Physical}, m) = (currentstate(obj).mass = m; obj)
# setposition!(obj::Union{Physical}, x, y) = (obj.x = x; obj.y = y; obj)
# setvelocity!(obj::Union{Physical}, vx, vy) = (obj.vx = vx; obj.vy = vy; obj)
# setposition!(obj::Union{Physical}, p::SVector{2}) = (obj.x = p[1]; obj.y = p[2]; obj)
# setvelocity!(obj::Union{Physical}, v::SVector{2}) = (obj.vx = v[1]; obj.vy = v[2]; obj)
# setangularmass!(obj::Union{Physical}, I) = (obj.I = I; obj)
# setangle!(obj::Union{Physical}, θ) = (obj.θ = θ; obj)
# setangularvelocity!(obj::Union{Physical}, ω) = (obj.ω = ω; obj)

for name in [:mass, :angularmass, :position, :angle, :velocity, :angularvelocity]
    @eval function $name(obj::Physical)
        return currentstate(obj).$name
    end

    @eval function $(Symbol("set$(name)!"))(obj::Physical, val)
        currentstate(obj).$name = val
        return obj
    end
end

translate!(obj::Physical, val::SVector) = setposition!(obj, position(obj) + val)
rotate!(obj::Physical, val::SVector) = setangle!(obj, angle(obj) + val)

# """
#     applyimpulse!(obj, impulse, point)

# Apply to `obj` an `impulse` at `point`.

# """
# function applyimpulse!(obj::Union{Physical,PhysicalState}, j::Vertex, p::Vertex)
#     m = mass(obj)
#     I = angularmass(obj)
#     r = p - position(obj)

#     obj.vx += j.x/m
#     obj.vy += j.y/m
#     obj.ω += (r × j)/I
# end

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
        timeevolve!(obj, dt)
    end
    return obj
end

##################################################
# State functions
##################################################

function savestate!(obj::Physical, t::Float64)
    push!(obj.state_history, copy(currentstate(obj)))
    currentstate(obj).t = t
    return obj
end

##################################################
# Time translation
##################################################

function timeevolve!(obj::Physical, dt::Float64)
    if !isstatic(obj)
        translate!(obj, dt*velocity(obj))
        rotate!(obj, dt*angularvelocity(obj))
    end
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

##################################################
# Collisions
##################################################

struct Collision{D, T1<:Physical, T2<:Physical, T} <: AbstractPhysicalObject
    t::Float64
    obj1::T1
    obj2::T2
    p::SVector{D,T}
    n::SVector{D,T}
    d::T
    CR::Float64
end
# Base.:(==)(col1::Collision, col2::Collision) =
#     # col1.t == col2.t &&
#     # col1.p == col2.p &&
#     # col1.n == col2.n &&
#     # col1.d == col2.d &&
#     # col1.CR == col2.CR &&
#     ((col1.obj1 == col2.obj1 && col1.obj2 == col2.obj2) ||
#      (col1.obj1 == col2.obj2 && col1.obj2 == col2.obj1))
Base.isless(col1::Collision, col2::Collision) = col1.d < col2.d

intersects(obj1::Physical, obj2::Physical) = intersects(shape(obj1), shape(obj2))
# inside(obj1::Physical, p::Vertex, θ) = inside(shape(obj1), p, θ)

"""
    collisions(layer1, layer2, t, dt)

Return a sorted array of collisions that happened during the last
update step between objects in `layer1` and `layer2`

"""
function collisions(layer1::AbstractLayer, layer2::AbstractLayer, t::Float64, dt::Float64)
    # TODO: broad phase / narrow phase separation
    cs = Collision[]
    for (i, obj1) in enumerate(layer1.objects), (j, obj2) in enumerate(layer2.objects)
        (i <= j || (isstatic(obj1) && isstatic(obj2))) && continue
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
            push!(cs, Collision(t, obj1, obj2, p, n, d, restitution(obj1, obj2)))
        else
            d, n, p = d2, n2, Shapes.vector(Shapes.supportvertex(shape1, n2))
            push!(cs, Collision(t, obj2, obj1, p, n, d, restitution(obj2, obj1)))
        end
    end
    return cs
end

##################################################
# Collision model
##################################################

restitution(::Physical, ::Physical) = 0.

struct CollisionLayer{T1 <: AbstractLayer, T2 <: AbstractLayer} <: AbstractLayer{AbstractPhysicalObject}
    source_layer::T1
    destination_layer::T2
    objects::OrderedSet{AbstractPhysicalObject}
    x::Float64
    y::Float64
    θ::Float64
    scale::Float64
    show::Bool
    render_tasks::Vector{RenderTask}
    new_objects::OrderedSet{AbstractPhysicalObject}
    dead_objects::Set{AbstractPhysicalObject}

    function CollisionLayer(source_layer::T1, destination_layer::T2, x = 0.0, y = 0.0, θ = 0.0; show = false, scale = 1.0) where {T1<:AbstractLayer,T2<:AbstractLayer}
        return new{T1,T2}(
            source_layer, destination_layer, OrderedSet{AbstractPhysicalObject}(), x, y, θ, scale, show,
            RenderTask[], OrderedSet{AbstractPhysicalObject}(), Set{AbstractPhysicalObject}())
    end
end

visible(layer::CollisionLayer) = layer.show

function before_update!(layer::CollisionLayer, ::AbstractScene, t::Float64, dt::Float64)
    populate!(layer)
    for obj in layer.objects
        before_update!(obj, layer, t, dt)
    end
    # Solve constraints (first pass)

    # Solve collisions iteratively
    k = 5
    for _ in 1:k
        for c in collisions(layer.source_layer, layer.destination_layer, t, dt)
            solve!(c)
        end
    end
    return layer

    # Solve constrants (second pass)
end

function solve!(self::Collision)
    j, Δx, #=Δθ,=# Δv, Δω = collide(self.obj1, self.obj2, self.n, self.p, self.d, self.CR)
    # println("$j, $(Δx)")
    if j >= 0
        translate!(self.obj1, Δx[1])
        translate!(self.obj2, Δx[2])
        # rotate!(self.obj1, Δθ[1])
        # rotate!(self.obj2, Δθ[2])
        setvelocity!(self.obj1, velocity(self.obj1) + Δv[1])
        setvelocity!(self.obj2, velocity(self.obj2) + Δv[2])
        setangularvelocity!(self.obj1, angularvelocity(self.obj1) + Δω[1])
        setangularvelocity!(self.obj2, angularvelocity(self.obj2) + Δω[2])
    end
end

"""
    collide(obj1, obj2, n, p, d, CR)

Return a tuple, `((Δx1, Δx2), (Δv1, Δv2), (Δω1, Δω2))`, where `Δx[i]`,
`Δv[i]`, `Δω[i]` is the required displacement and the change in
velocity and angular velocity after the collision of `obj1` and
`obj2`, respectively.

"""
function collide(obj1::Physical, obj2::Physical, n::SVector, p::SVector, d::Real, CR::Real)
    m1, m2 = isstatic(obj1) ? Inf : mass(obj1)[1], isstatic(obj2) ? Inf : mass(obj2)[1]
    I1, I2 = isstatic(obj1) ? Inf : angularmass(obj1)[1], isstatic(obj2) ? Inf : angularmass(obj2)[1]
    p1, p2 = position(obj1), position(obj2)
    θ1, θ2 = angle(obj1), angle(obj2)
    v1, v2 = velocity(obj1), velocity(obj2)
    ω1, ω2 = angularvelocity(obj1)[1], angularvelocity(obj2)[1]

    r1 = p - p1
    r2 = p - p2
    Δv = (v2 + ω2*(r2 |> rotate(90))*pi/180) - (v1 + ω1*(r1 |> rotate(90))*pi/180)

    j = -(1 + CR) * (Δv ⋅ n)
    A1 = r1 × n
    A2 = r2 × n
    f = 1 / (inv(m1) + inv(m2) + inv(I1)*A1^2 + inv(I2)*A2^2)
    fm = 1 / (inv(m1) + inv(m2))

    return (
        j,
        (-f/m1*n*d, f/m2*n*d),
        # (-SVector(f*A1/I1*180/pi*d), SVector(f*A2/I2*180/pi*d)),
        (-j*f/m1*n, j*f/m2*n),
        (SVector(-j*f*A1/I1*180/pi), SVector(j*f*A2/I2*180/pi))
    )
end

function render!(layer::CollisionLayer, self::Collision, frame::Int, fps::Float64)
    render!(layer, Shapes.Polyline(Shapes.Vertex(self.p...), Shapes.Vertex((self.p |> translate((2*self.d)*self.n...))...)), 0., 0., 0., color=colorant"#F00")
    render!(layer, Shapes.Polyline(Shapes.Vertex(self.p...), Shapes.Vertex((self.p |> translate((self.d)*self.n...))...)), 0., 0., 0., color=colorant"#00F")
end

end #module
