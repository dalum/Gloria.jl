module Physics

using Colors
using DataStructures: CircularBuffer, OrderedSet, SortedSet
using LinearAlgebra

using Gloria: AbstractObject, AbstractLayer, AbstractScene, RenderTask,
    add!, kill!, populate!
using ..Shapes: AbstractShape, Line, NonPrimitiveShape, Point,
    closestprojection, extrude, rotate, trace, tracepoint, tracepointnormal, translate, vertices

export CollisionLayer, Physical,
    collide!, oncollision!, timeintersects

import Gloria: after_update!, before_update!, render!, update!, visible
import ..Shapes: inside, intersects

const flatten = Iterators.flatten

const COLLISION_TIME_OFFSET = 1e-5
const MAX_COLLISION_DEPTH = 500

const Vec{T} = Point{T}

abstract type AbstractPhysicalState end

mutable struct PhysicalState{T} <: AbstractPhysicalState
    t::T
    static::Bool
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
    PhysicalState(0., static, m, I, x, y, θ, vx, vy, ω)

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

tocoordinates(obj::Physical, p::Point) = p |> translate(-obj.x, -obj.y) |> rotate(-obj.θ)

Base.size(obj::Physical) = sqrt(maximum(p -> p.x^2 + p.y^2, vertices(obj.shape)))

shape(obj::Physical) = obj.shape |> rotate(obj.θ) |> translate(obj.x, obj.y)
wrapped(obj::Physical) = obj.wrapped
isstatic(obj::Physical) = obj.static

mass(obj::Union{Physical,PhysicalState}) = obj.m
position(obj::Union{Physical,PhysicalState}) = Vec(obj.x, obj.y)
velocity(obj::Union{Physical,PhysicalState}) = Vec(obj.vx, obj.vy)
angularmass(obj::Union{Physical,PhysicalState}) = obj.I
angle(obj::Union{Physical,PhysicalState}) = obj.θ
angularvelocity(obj::Union{Physical,PhysicalState}) = obj.ω

setmass!(obj::Union{Physical,PhysicalState}, m) = (obj.m = m; obj)
setposition!(obj::Union{Physical,PhysicalState}, x, y) = (obj.x = x; obj.y = y; obj)
setvelocity!(obj::Union{Physical,PhysicalState}, vx, vy) = (obj.vx = vx; obj.vy = vy; obj)
setposition!(obj::Union{Physical,PhysicalState}, p::Point) = (obj.x = p.x; obj.y = p.y; obj)
setvelocity!(obj::Union{Physical,PhysicalState}, v::Vec) = (obj.vx = v.x; obj.vy = v.y; obj)
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
function applyimpulse!(obj::Union{Physical,PhysicalState}, j::Vec, p::Point)
    v = velocity(obj)
    ω = angularvelocity(obj)
    m = mass(obj)
    I = angularmass(obj)
    r = p - position(obj)
    v += j / m
    ω += (r × j)/I
    setvelocity!(obj, v)
    setangularvelocity!(obj, ω)
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

function timetracepoint(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    dt = state1.t - prevstate1.t
    tps = tracepoint(timecapture(shape1, shape2, (state1, prevstate1), (state2, prevstate2))...)
    return ((dt * (t - 1.), p) for (t, p) in tps)
end

function timetracepointnormal(shape1::AbstractShape, shape2::AbstractShape, (state1, prevstate1)::Tuple{PhysicalState, PhysicalState}, (state2, prevstate2)::Tuple{PhysicalState, PhysicalState})
    dt = state1.t - prevstate1.t
    return ((dt * (t - 1.), p, n) for (t, p, n) in tracepointnormal(timecapture(shape1, shape2, (state1, prevstate1), (state2, prevstate2))...))
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

function timetracepoint(obj1::Physical, obj2::Physical)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timetracepoint(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
end

function timetracepointnormal(obj1::Physical, obj2::Physical)
    state1, state2 = currentstate(obj1), currentstate(obj2)
    prevstate1, prevstate2 = obj1.state_history[end-1], obj2.state_history[end-1]
    return timetracepointnormal(obj1.shape, obj2.shape, (state1, prevstate1), (state2, prevstate2))
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
angle(v::Vec) = 180*atan(v.y, v.x)/π
function impulse(s::AbstractSpring, dt)
    d = position(s.hinge1) - position(s.hinge2)
    d1 = previousposition(s.hinge1) - previousposition(s.hinge2)
    nd = norm(d)
    return iszero(nd) ? Point(0.0, 0.0) : (-s.k + s.kd*(((d - d1)/dt) ⋅ d)/nd^2)*d*dt
end

##################################################
# Collisions
##################################################

struct Collision{T1 <: Physical, T2 <: Physical, T}
    obj1::T1
    obj2::T2
    t::Float64
    p1::Point{T}
    p2::Point{T}
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
    cs = Collision[]
    for obj1 in layer1.objects, obj2 in layer2.objects
        (obj1 === obj2 || isstatic(obj1) && isstatic(obj2)) && continue
        for p1 in unique!(collect(vertices(shape(obj1))))
            if inside(shape(obj2), p1, 270.009)
                p2 = closestprojection(shape(obj2), p1)
                push!(cs, Collision(obj1, obj2, t, p1, p2))
            end
        end
    end
    return cs
end

##################################################
# Collision penalty model
##################################################

struct CollisionLayer{T1 <: AbstractLayer, T2 <: AbstractLayer} <: AbstractLayer{AbstractPhysicalObject}
    layer1::T1
    layer2::T2
    objects::OrderedSet{AbstractPhysicalObject}
    x::Float64
    y::Float64
    θ::Float64
    scale::Float64
    show::Bool
    render_tasks::Vector{RenderTask}
    new_objects::OrderedSet{AbstractPhysicalObject}
    dead_objects::Set{AbstractPhysicalObject}

    function CollisionLayer(layer1::T1, layer2::T2, x = 0.0, y = 0.0, θ = 0.0; show = false, scale = 1.0) where {T1<:AbstractLayer,T2<:AbstractLayer}
        return new{T1,T2}(
            layer1, layer2, OrderedSet{AbstractPhysicalObject}(), x, y, θ, scale, show,
            RenderTask[], OrderedSet{AbstractPhysicalObject}(), Set{AbstractPhysicalObject}())
    end
end

visible(layer::CollisionLayer) = layer.show

struct CollisionPenaltySpring{T} <: AbstractSpring{T}
    hinge1::Hinge{T}
    hinge2::Hinge{T}
    k::T
    kd::T
    l0::T
    t::Float64
    lifetime::Float64
end

function render!(layer::CollisionLayer, self::CollisionPenaltySpring, frame::Int, fps::Float64)
    p1 = position(self.hinge1)
    p2 = position(self.hinge2)
    render!(layer, Line(p1, p2), 0., 0., 0., color=colorant"#0F0")
end

function before_update!(layer::CollisionLayer, ::AbstractScene, t::Float64, dt::Float64)
    populate!(layer)
    for obj in layer.objects
        before_update!(obj, layer, t, dt)
    end
    for c in collisions(layer.layer1, layer.layer2, t, dt)
        # println(c)
        # println("$(c.p1), $(c.p2)")
        add!(layer, CollisionPenaltySpring(
            Hinge(c.obj1, tocoordinates(c.obj1, c.p1)),
            Hinge(c.obj2, tocoordinates(c.obj2, c.p2)),
            10000., -100., 0., t, 0.01))
    end
    return layer
end

function update!(self::CollisionPenaltySpring, layer::AbstractLayer, t::Float64, dt::Float64)
    j = impulse(self, dt)
    r = min(norm(j), 3000.) / norm(j)
    applyimpulse!(self.hinge1.obj, j*r, position(self.hinge1))
    applyimpulse!(self.hinge2.obj, -j*r, position(self.hinge2))
    if t > self.t + self.lifetime
        kill!(layer, self)
    end
end

end #module
