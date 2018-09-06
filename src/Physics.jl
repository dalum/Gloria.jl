module Physics

using Gloria: AbstractObject, AbstractShape, Layer

import Gloria: after_update!, before_update!, render!, update!,
    extrude, intersects, transform, simplest

abstract type PhysicalObject <: AbstractObject end

"""

"""
mutable struct Physical{T <: AbstractObject} <: PhysicalObject
    wrapped::T
    shape::AbstractShape
    m::Float64
    I::Float64
    x::Float64
    y::Float64
    θ::Float64
    vx::Float64
    vy::Float64
    ω::Float64
end
Physical(wrapped::T, shape; m=0., I=0., x=0., y=0., θ=0., vx=0., vy=0., ω=0.) where {T} = Physical(wrapped, shape, m, I, x, y, θ, vx, vy, ω)

@inline Base.getproperty(obj::Physical, v::Symbol) = _getproperty(obj, Val(v))
@inline _getproperty(obj::Physical, ::Val{V}) where V = Core.getfield(obj.wrapped, V)
for v in [:(:wrapped), :(:shape), :(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
    @eval @inline _getproperty(obj::Physical, ::Val{$v}) = Core.getfield(obj, $v)
end

@inline Base.setproperty!(obj::Physical, v::Symbol, x) = _setproperty!(obj, Val(v), x)
@inline _setproperty!(obj::Physical, ::Val{V}, x) where V = Core.setfield!(obj.wrapped, V, x)
for v in [:(:wrapped), :(:shape), :(:m), :(:I), :(:x), :(:y), :(:θ), :(:vx), :(:vy), :(:ω)]
    @eval @inline _setproperty!(obj::Physical, ::Val{$v}, x) = Core.setfield!(obj, $v, x)
end

wrapped(obj::Physical) = obj.wrapped

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

function translate!(obj::PhysicalObject, x′, y′)
    x, y = position(obj)
    setposition!(obj, x + x′, y + y′)
end

rotate!(obj::PhysicalObject, θ′) = setangle!(obj, angle(obj) + θ′)

function interact!(::PhysicalObject, ::PhysicalObject; t, dt) end

##################################################
# Gloria overrides
##################################################

function before_update!(obj::PhysicalObject; t::Float64, dt::Float64)
    vx, vy = velocity(obj)
    ω = angularvelocity(obj)
    translate!(obj, dt*vx, dt*vy)
    rotate!(obj, dt*ω)
end

function update!(layer::Layer{<:Physical}; t::Float64, dt::Float64)
    for obj1 in layer.objects
        for obj2 in layer.objects
            if obj1 === obj2
                update!(obj1, t=t, dt=dt)
            else
                interact!(obj1, obj2, t=t, dt=dt)
            end
        end
    end
    return layer
end

intersects(obj1::PhysicalObject, obj2::PhysicalObject) = intersects(transform(obj1.shape, obj1.x, obj1.y, obj1.θ), transform(obj2.shape, obj2.x, obj2.y, obj2.θ))

function intersects(obj1::PhysicalObject, obj2::PhysicalObject, dt::Float64)
    (sobj, aobj) = simplest(obj1.shape, obj2.shape) === obj1.shape ? (obj1, obj2) : (obj2, obj1)
    Δx, Δy = sobj.x - aobj.x, sobj.y - aobj.y
    vxdt = -(sobj.vx - aobj.vx + Δy*aobj.ω*π/180)*dt
    vydt = -(sobj.vy - aobj.vy - Δx*aobj.ω*π/180)*dt
    sshape = transform(sobj.shape, 0., 0., sobj.θ)
    sshape = extrude(sshape, vxdt, vydt, -sobj.ω*dt)
    sshape = transform(sshape, Δx, Δy, 0.)
    ashape = transform(aobj.shape, 0, 0, aobj.θ)
    return intersects(sshape, ashape)
end

end #module
