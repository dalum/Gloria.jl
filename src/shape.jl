abstract type AbstractShape end

struct Point{T} <: AbstractShape
    x::T
    y::T
end

struct Line{T} <: AbstractShape
    x1::T
    y1::T
    x2::T
    y2::T
    t_min::T
    t_max::T
end
Line(p1::Point{T}, p2::Point{T}) where T = Line(p1.x, p1.y, p2.x, p2.y, zero(T), one(T))
Line(x1::T, y1::T, x2::T, y2::T) where T = Line(x1, y1, x2, y2, zero(T), one(T))
Line(x::T, y::T, θ::T; t_min::T = convert(T, -Inf), t_max::T = convert(T, Inf)) where T = Line(x, y, x + cos(θ), y + sin(θ), t_min, t_max)

struct Circle{T} <: AbstractShape
    x::T
    y::T
    r::T
end

struct Polygon{N,T} <: AbstractShape
    points::Vector{Point{T}}
    lines::Vector{Line{T}}
end
function Polygon(point::Point{T}, points::Point{T}...) where {T}
    lines = Line{T}[]
    p0 = point
    for p1 in points[1:end]
        push!(lines, Line(p0.x, p0.y, p1.x, p1.y))
        p0 = p1
    end
    if length(points) > 1
        push!(lines, Line(p0.x, p0.y, point.x, point.y))
    end
    points_ = [point]
    append!(points_, collect(points))
    return Polygon{length(points_),T}(points_, lines)
end

struct CompositeShape{T<:AbstractShape} <: AbstractShape
    shapes::Vector{T}
end


"""
    points(shape)
"""
points(p::Point) = [p]
points(l::Line) = [Point(l.x1, l.y1), Point(l.x2, l.y2)]
points(m::Polygon) = m.points
points(s::CompositeShape) = vcat([points(shape) for shape in s.shapes]...)

"""
    simplest(shape1, shape2)
"""
simplest(p::Point, ::Point) = p
simplest(p::Point, ::Line) = p
simplest(p::Point, ::Polygon) = p
simplest(p::Point, ::Circle) = p
simplest(p::Point, ::CompositeShape) = p

simplest(::Line, p::Point) = p
simplest(l::Line, ::Line) = l
simplest(l::Line, m::Polygon{N}) where {N} = N < 2 ? m : l
simplest(l::Line, ::Circle) = l
simplest(l::Line, ::CompositeShape) = l

simplest(::Polygon, p::Point) = p
simplest(m::Polygon{N}, l::Line) where {N} = N < 2 ? m : l
simplest(m1::Polygon{N}, m2::Polygon{M}) where {N,M} = N < M ? m1 : m2
simplest(m::Polygon, ::Circle) = m
simplest(m::Polygon, ::CompositeShape) = m

simplest(::Circle, p::Point) = p
simplest(::Circle, l::Line) = l
simplest(::Circle, m::Polygon) = m
simplest(c::Circle, ::Circle) = c
simplest(::Circle, cs::CompositeShape) = cs

simplest(::CompositeShape, p::Point) = p
simplest(::CompositeShape, l::Line) = l
simplest(::CompositeShape, m::Polygon) = m
simplest(cs::CompositeShape, ::CompositeShape) = cs
simplest(cs::CompositeShape, ::Circle) = cs

"""
    transform(shape)
"""
transform(p::Point, x, y, θ) = Point(x + cosd(θ)*p.x - sind(θ)*p.y, y + sind(θ)*p.x + cosd(θ)*p.y)
transform(c::Circle, x, y, θ) = Circle(x + cosd(θ)*c.x - sind(θ)*c.y, y + sind(θ)*c.x + cosd(θ)*c.y, c.r)
transform(l::Line, x, y, θ) = Line(x + cosd(θ)*l.x1 - sind(θ)*l.y1, y + sind(θ)*l.x1 + cosd(θ)*l.y1, x + cosd(θ)*l.x2 - sind(θ)*l.y2, y + sind(θ)*l.x2 + cosd(θ)*l.y2, l.t_min, l.t_max)
transform(m::Polygon{N,T}, x, y, θ) where {N,T} = Polygon{N,T}(transform.(m.points, x, y, θ), transform.(m.lines, x, y, θ))
transform(cs::CompositeShape, x, y, θ) = CompositeShape(transform.(cs.shapes, x, y, θ))

"""
    extrude(shape, x, y, θ)
"""
extrude(p::Point, x, y, θ) = Line(p, transform(p, x, y, θ))

function extrude(l1::Line{T}, x, y, θ) where T
    l2 = transform(l1, x, y, θ)
    p1, p2 = points(l1)
    p3, p4 = points(l2)
    l3 = Line(p1, p3)
    l4 = Line(p2, p4)
    return Polygon{4,T}([p1, p2, p3, p4], [l1, l2, l3, l4])
end

extrude(m::Polygon, x, y, θ) = CompositeShape(extrude.(m.lines, x, y, θ))
extrude(cs::CompositeShape, x, y, θ) = CompositeShape(extrude.(cs.shapes, x, y, θ))

# Poor man's extrusion
function extrude(c::Circle, x, y, θ; subsamples::Int = 4)
    dx = x / subsamples
    dy = y / subsamples
    dθ = θ / subsamples
    return CompositeShape([transform(c, dx*i, dy*i, dθ*i) for i in 0:subsamples])
end

"""
    intersects(shape1, shape2)
"""
intersects(p1::Point, p2::Point) = p1.x == p2.x && p1.y == p2.y
intersects(c1::Circle, c2::Circle) = sqrt((c2.x - c1.x)^2 + (c2.y - c1.y)^2) <= c1.r + c2.r
intersects(p::Point, c::Circle) = sqrt((c.x - p.x)^2 + (c.y - p.y)^2) <= c.r

intersects(l::Line, p::Point) = intersects(p, l)
function intersects(p::Point, l::Line)
    a1 = (l.x2 - l.x1)
    a2 = (l.y2 - l.y1)
    b1 = (p.x - l.x1)
    b2 = (p.y - l.y1)

    d1 = b1 / a1
    d2 = b2 / a2

    a1 == b1 == 0 && return l.t_min <= d2 <= l.t_max
    a2 == b2 == 0 && return l.t_min <= d1 <= l.t_max

    return d1 === d2 && l.t_min <= d1 <= l.t_max || l.t_min <= d2 <= l.t_max
end

function intersects(l1::Line, l2::Line)
    # From https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    t1 = (l1.x1 - l2.x1)*(l2.y1 - l2.y2) - (l1.y1 - l2.y1)*(l2.x1 - l2.x2)
    t2 = (l1.x1 - l2.x1)*(l1.y1 - l1.y2) - (l1.y1 - l2.y1)*(l1.x1 - l1.x2)
    d = (l1.x1 - l1.x2)*(l2.y1 - l2.y2) - (l1.y1 - l1.y2)*(l2.x1 - l2.x2)

    # Parallel lines
    if iszero(t1) && iszero(t2) && iszero(d)
        return intersects(l1, Point(l2.x1, l2.y1)) || intersects(l1, Point(l2.x2, l2.y2))
    end

    return l1.t_min <= t1/d <= l1.t_max && l2.t_min <= t2/d <= l2.t_max
end

intersects(c::Circle, l::Line) = intersects(l, c)
function intersects(l::Line, c::Circle)
    # From https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
    l² = (l.x2 - l.x1)^2 + (l.y2 - l.y1)^2
    a = ((l.x2 - l.x1)*(l.x1 - c.x) + (l.y2 - l.y1)*(l.y1 - c.y))
    b = (l.x1 - c.x)^2 + (l.y1 - c.y)^2 - c.r^2

    a^2 - l²*b < 0 && return false

    d1 = (-a + sqrt(a^2 - l²*b)) / l²
    d2 = (-a - sqrt(a^2 - l²*b)) / l²

    return l.t_min <= d1 <= l.t_max || l.t_min <= d2 <= l.t_max
end

intersects(m::Polygon, a::Union{<:Point,<:Line,<:Circle}) = intersects(a, m)
intersects(cs::CompositeShape, a::Union{<:Point,<:Line,<:Circle}) = intersects(a, cs)
intersects(a::Union{<:Point,<:Line,<:Circle}, m::Polygon) = any(l->intersects(a, l), m.lines)
intersects(a::Union{<:Point,<:Line,<:Circle}, cs::CompositeShape) = any(s->intersects(a, s), cs.shapes)

intersects(m1::Polygon, m2::Polygon) = any(l->intersects(l, m2), m1.lines)
intersects(m::Polygon, cs::CompositeShape) = any(l->intersects(l, cs), m.lines)
intersects(cs::CompositeShape, m::Polygon) = intersects(m, cs)
intersects(cs1::CompositeShape, cs2::CompositeShape) = any(s->intersects(s, cs2), cs1.shapes)
