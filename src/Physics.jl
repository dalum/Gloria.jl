module Physics

using Colors
using DataStructures: CircularBuffer, OrderedSet, SortedSet
using LinearAlgebra

using Gloria: AbstractObject, AbstractLayer, AbstractScene, RenderTask,
    add!, kill!, populate!
using ..Shapes: AbstractShape, BoundingBox, Point,
    closestprojection, edges, extrude, rotate, trace, translate, vertices

export CollisionLayer, Physical,
    collide!, oncollision!, timeintersects

import Gloria: after_update!, before_update!, render!, update!, visible
import ..Shapes: inside, intersects, subdivide

const flatten = Iterators.flatten

const COLLISION_TIME_OFFSET = 1e-5
const MAX_COLLISION_DEPTH = 500

abstract type AbstractPhysicalState end

mutable struct PhysicalState{T} <: AbstractPhysicalState
    t::T
    static::Bool
    awake::Bool
    ncontacts::Int
    m::T
    I::T
    x::T
    y::T
    θ::T
    vx::T
    vy::T
    ω::T
end
PhysicalState(static::Bool, m, I, x, y, θ, vx, vy, ω) =
    PhysicalState(0., static, true, 0, m, I, x, y, θ, vx, vy, ω)

Base.copy(state::PhysicalState) = PhysicalState(
    state.t,
    state.static,
    state.awake,
    state.ncontacts,
    state.m,
    state.I,
    state.x,
    state.y,
    state.θ,
    state.vx,
    state.vy,
    state.ω
)

abstract type AbstractPhysicalObject <: AbstractObject end

"""

"""
mutable struct Physical{T1 <: AbstractObject, T2 <: AbstractShape, T3 <: AbstractPhysicalState} <: AbstractPhysicalObject
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
    Physical(wrapped, shape, PhysicalState(static, convert.(Float64, (m, I, x, y, θ, vx, vy, ω))...))
Physical(wrapped, shape; static=false, m=1., I=1., x=0., y=0., θ=0., vx=0., vy=0., ω=0.) =
    Physical(wrapped, shape, static, m, I, x, y, θ, vx, vy, ω)

@inline currentstate(obj::Physical) = Core.getfield(obj, :state_history)[end]
@inline previousstate(obj::Physical) = Core.getfield(obj, :state_history)[end-1]

@inline Base.getproperty(obj::Physical, v::Symbol) = _getproperty(obj, Val(v))
@inline _getproperty(obj::Physical, ::Val{V}) where V = Core.getfield(obj.wrapped, V)
for v in [:(:wrapped), :(:shape), :(:state_history)]
    @eval @inline _getproperty(obj::Physical, ::Val{$v}) = Core.getfield(obj, $v)
end
for v in [:(:static), :(:awake), :(:ncontacts), :(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
    @eval @inline _getproperty(obj::Physical, ::Val{$v}) = Core.getfield(currentstate(obj), $v)
end

@inline Base.setproperty!(obj::Physical, v::Symbol, x) = _setproperty!(obj, Val(v), x)
@inline _setproperty!(obj::Physical, ::Val{V}, x) where V = Core.setfield!(obj.wrapped, V, x)
for v in [:(:wrapped), :(:shape), :(:state_history)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(obj, $v, x)
end
for v in [:(:static), :(:awake), :(:ncontacts)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(currentstate(obj), $v, x)
end
for v in [:(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(currentstate(obj), $v, convert(Float64, x))
end

tocoordinates(obj::Physical, p::Point) = p |> translate(-obj.x, -obj.y) |> rotate(-obj.θ)

Base.size(obj::Physical) = sqrt(maximum(p -> p.x^2 + p.y^2, vertices(obj.shape)))

shape(obj::Physical) = obj.shape |> rotate(obj.θ) |> translate(obj.x, obj.y)
wrapped(obj::Physical) = obj.wrapped
isstatic(obj::Physical) = obj.static

mass(obj::Union{Physical,PhysicalState}) = obj.m
position(obj::Union{Physical,PhysicalState}) = Point(obj.x, obj.y)
velocity(obj::Union{Physical,PhysicalState}) = Point(obj.vx, obj.vy)
angularmass(obj::Union{Physical,PhysicalState}) = obj.I
angle(obj::Union{Physical,PhysicalState}) = obj.θ
angularvelocity(obj::Union{Physical,PhysicalState}) = obj.ω

setmass!(obj::Union{Physical,PhysicalState}, m) = (obj.m = m; obj)
setposition!(obj::Union{Physical,PhysicalState}, x, y) = (obj.x = x; obj.y = y; obj)
setvelocity!(obj::Union{Physical,PhysicalState}, vx, vy) = (obj.vx = vx; obj.vy = vy; obj)
setposition!(obj::Union{Physical,PhysicalState}, p::Point) = (obj.x = p.x; obj.y = p.y; obj)
setvelocity!(obj::Union{Physical,PhysicalState}, v::Point) = (obj.vx = v.x; obj.vy = v.y; obj)
setangularmass!(obj::Union{Physical,PhysicalState}, I) = (obj.I = I; obj)
setangle!(obj::Union{Physical,PhysicalState}, θ) = (obj.θ = θ; obj)
setangularvelocity!(obj::Union{Physical,PhysicalState}, ω) = (obj.ω = ω; obj)

@inline function translate!(obj::Union{Physical,PhysicalState}, x′, y′)
    p = position(obj)
    setposition!(obj, p.x + x′, p.y + y′)
end

rotate!(obj::Union{Physical,PhysicalState}, θ′) = setangle!(obj, angle(obj) + θ′)

"""
    applyimpulse!(obj, impulse, point)

Apply to `obj` an `impulse` at `point`.

"""
function applyimpulse!(obj::Union{Physical,PhysicalState}, j::Point, p::Point)
    m = mass(obj)
    I = angularmass(obj)
    r = p - position(obj)

    obj.vx += j.x/m
    obj.vy += j.y/m
    obj.ω += (r × j)/I
end

momentum(obj::Union{Physical,PhysicalState}) = mass(obj).*velocity(obj)
angularmomentum(obj::Union{Physical,PhysicalState}) = angularmass(obj)*angularvelocity(obj)
kineticenergy(obj::Union{Physical,PhysicalState}) = 0.5mass(obj)*norm(velocity(obj))^2
angularkineticenergy(obj::Union{Physical,PhysicalState}) = 0.5angularmass(obj)*angularvelocity(obj)^2
totalkineticenergy(obj::Union{Physical,PhysicalState}) = kineticenergy(obj) + angularkineticenergy(obj)

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
        state = currentstate(obj)
        vx = state.vx
        vy = state.vy
        ω = state.ω
        translate!(obj, dt*vx, dt*vy)
        rotate!(obj, dt*ω)
    end
end

##################################################
# Time projection
##################################################

function timecapture(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    shape = shape1 |> rotate(prevstate1.θ) |> translate(prevstate1.x, prevstate1.y)
    lines = unique!(collect(vertices(shape2))) |> rotate(prevstate2.θ) |>
        extrude(rotate(state2.θ - prevstate2.θ) |> translate(state2.x - prevstate2.x, state2.y - prevstate2.y)) |>
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

abstract type AbstractHinge{T} <: AbstractPhysicalObject end
abstract type AbstractSpring{T} <: AbstractPhysicalObject end

struct Hinge{T} <: AbstractHinge{T}
    obj::Physical
    p::Point{T}
end

struct Spring{T} <: AbstractSpring{T}
    hinge1::Hinge{T}
    hinge2::Hinge{T}
    k::T
    l0::T
end

position(h::AbstractHinge) = h.p |> rotate(h.obj.θ) |> translate(h.obj.x, h.obj.y)
previousposition(h::AbstractHinge) = h.p |> rotate(previousstate(h.obj).θ) |> translate(previousstate(h.obj).x, previousstate(h.obj).y)
angle(v::Point) = 180*atan(v.y, v.x)/π

##################################################
# Collisions
##################################################

struct Collision{T1<:Physical, T2<:Physical, T} <: AbstractPhysicalObject
    t::Float64
    obj1::T1
    obj2::T2
    p1::Point{T}
    p2::Point{T}
    CR::Float64
end
Base.:(==)(col1::Collision, col2::Collision) =
    col1.t == col2.t && col1.p1 == col2.p1 && col1.p2 == col2.p2 &&
    ((col1.obj1 == col2.obj1 && col1.obj2 == col2.obj2) ||
     (col1.obj1 == col2.obj2 && col1.obj2 == col2.obj1))
Base.isless(col1::Collision, col2::Collision) = col1.t < col2.t

intersects(obj1::Physical, obj2::Physical) = intersects(shape(obj1), shape(obj2))
inside(obj1::Physical, p::Point, θ) = inside(shape(obj1), p, θ)

"""
    collisions(layer1, layer2, t, dt)

Return a sorted array of collisions that happened during the last
update step between objects in `layer1` and `layer2`

"""
function collisions(layer1::AbstractLayer, layer2::AbstractLayer, t::Float64, dt::Float64)
    # TODO: broad phase / narrow phase separation
    cs = Collision[]
    for obj1 in layer1.objects, obj2 in layer2.objects
        (obj1 === obj2 || isstatic(obj1) && isstatic(obj2)) && continue
        shape1 = shape(obj1)
        shape2 = shape(obj2)
        !intersects(BoundingBox(shape1), BoundingBox(shape2)) && continue
        # !!!TODO: Separating Axis Theorem!!!
        for p1 in vertices(shape1)
            if inside(shape2, p1, 270)
                p2 = closestprojection(shape2, p1)
                if p2 !== nothing
                    push!(cs, Collision(t, obj1, obj2, p1, p2, 0.65))
                end
            end
        end
        for e1 in edges(shape1), e2 in edges(shape2)
            for a in trace(e2, e1)
                p1 = e1[1] + (e1[2] - e1[1]) * a
                push!(cs, Collision(t, obj1, obj2, p, p, 0.65))
            end
        end
    end
    return cs
end

##################################################
# Collision model
##################################################

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
    for c in collisions(layer.source_layer, layer.destination_layer, t, dt)
        add!(layer, c)
    end
    populate!(layer)
    for obj in layer.objects
        before_update!(obj, layer, t, dt)
    end
    return layer
    # # Solve constraints (first pass)

    # # Find all collisions
    # cs = collisions(layer.source_layer, layer.destination_layer, t, dt)
    # # ...

    # # Solve collision positions
    # solvepositions!(cs)

    # # Solve constrants (second pass)

    # # Solve velocities
    # solvevelocities!(cs)
end

function before_update!(self::Collision, ::AbstractLayer, t::Float64, dt::Float64)
    self.obj1.ncontacts += 1
    self.obj2.ncontacts += 1
end

function update!(self::Collision, ::AbstractLayer, t::Float64, dt::Float64)
    m1, m2 = isstatic(self.obj1) ? Inf : mass(self.obj1), isstatic(self.obj2) ? Inf : mass(self.obj2)
    I1, I2 = isstatic(self.obj1) ? Inf : angularmass(self.obj1), isstatic(self.obj2) ? Inf : angularmass(self.obj2)
    p1, p2 = position(self.obj1), position(self.obj2)
    θ1, θ2 = angle(self.obj1), angle(self.obj2)
    v1, v2 = velocity(self.obj1), velocity(self.obj2)
    ω1, ω2 = angularvelocity(self.obj1), angularvelocity(self.obj2)

    r1 = self.p1 - p1
    r2 = self.p2 - p2
    Δv = (v1 + ω1 × r1*pi/180) - (v2 + ω2 × r2*pi/180)

    d = norm(self.p2 - self.p1)
    n = normalize(self.p2 - self.p1)
    j = -(1 + self.CR) * (Δv ⋅ n)
    A1 = r1 × n
    A2 = r2 × n
    f = 1 / (inv(m1) + inv(m2) + inv(I1)*A1^2 + inv(I2)*A2^2)
    fm = 1 / (inv(m1) + inv(m2))

    ρ = 0.85
    if j >= 0
        setposition!(self.obj1, p1 + f/m1*n*d*ρ)
        setposition!(self.obj2, p2 - f/m2*n*d*ρ)
        setangle!(self.obj1, θ1 + f/I1*A1*180/pi*d*ρ)
        setangle!(self.obj2, θ2 - f/I2*A2*180/pi*d*ρ)
        setangularvelocity!(self.obj1, ω1 + j*f*A1/I1*180/pi)
        setangularvelocity!(self.obj2, ω2 - j*f*A2/I2*180/pi)
        setvelocity!(self.obj1, v1 + j*f/m1*n)
        setvelocity!(self.obj2, v2 - j*f/m2*n)
        # setvelocity!(self.obj1, 0v1)
        # setvelocity!(self.obj2, 0v2)
    end
end

function after_update!(self::Collision, layer::AbstractLayer, t::Float64, dt::Float64)
    self.obj1.ncontacts -= 1
    self.obj2.ncontacts -= 1
    kill!(layer, self)
end

# add!(layer, CollisionPenaltySpring(
#     Hinge(c.obj1, tocoordinates(c.obj1, c.p1)),
#     Hinge(c.obj2, tocoordinates(c.obj2, c.p2)),
#     10000., 100000., t, 0.01))

# struct CollisionPenaltySpring{T} <: AbstractSpring{T}
#     hinge1::Hinge{T}
#     hinge2::Hinge{T}
#     k::T
#     kd::T
#     t::Float64
#     lifetime::Float64
# end

# function render!(layer::CollisionLayer, self::CollisionPenaltySpring, frame::Int, fps::Float64)
#     p1 = position(self.hinge1)
#     p2 = position(self.hinge2)
#     render!(layer, (p1, p2), 0., 0., 0., color=colorant"#0F0")
# end

# function update!(self::CollisionPenaltySpring, layer::AbstractLayer, t::Float64, dt::Float64)
#     if t > self.t + self.lifetime
#         kill!(layer, self)
#     end

#     d = position(self.hinge1) - position(self.hinge2)
#     n = norm(d)
#     d1 = previousposition(self.hinge1) - previousposition(self.hinge2)

#     v = (1 - (d1 ⋅ d))/dt/n^2
#     ds = t < self.t + self.lifetime ? dt + self.t + self.lifetime - t : dt
#     j = (iszero(n) || iszero(dt)) ? Point(0.0, 0.0) : -(self.k + (self.kd/self.k)*v)*d*ds
#     n = norm(j)
#     r = iszero(n) ? 0. : min(n, 1000dt/self.lifetime)/n

#     applyimpulse!(self.hinge1.obj, j*r, position(self.hinge1))
#     applyimpulse!(self.hinge2.obj, -j*r, position(self.hinge2))
# end

end #module
