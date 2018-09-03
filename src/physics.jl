"""

"""
mutable struct PhysicalObject{T <: AbstractTexture, S <: AbstractShape} <: AbstractObject
    texture::T
    shape::S
    #mass
    #angularmass
    x::Float64
    y::Float64
    θ::Float64
    vx::Float64
    vy::Float64
    ω::Float64
end
PhysicalObject(texture, shape, x=0., y=0., θ=0., vx=0., vy=0., ω=0.) = PhysicalObject(texture, shape, x, y, θ, vx, vy, ω)

render!(layer::Layer, obj::PhysicalObject; kwargs...) = render!(layer, obj.texture, obj.x, obj.y, obj.θ; kwargs...)

intersects(obj1::PhysicalObject, obj2::PhysicalObject) = intersects(transform(obj1.shape, obj1.x, obj1.y, obj1.θ), transform(obj2.shape, obj2.x, obj2.y, obj2.θ))

function update!(obj::PhysicalObject; dt::Float64)
    obj.x += dt*obj.vx
    obj.y += dt*obj.vy
    obj.θ += dt*obj.ω
end

function accelerate!(obj::PhysicalObject, ax, ay, α=0.; dt::Float64)
    obj.vx += dt*ax
    obj.vy += dt*ay
    obj.ω += dt*α
end
