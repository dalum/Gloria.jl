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
end

struct Circle{T} <: AbstractShape
    x::T
    y::T
    r::T
end

struct Polygon{T} <: AbstractShape
    lines::Vector{Line{T}}
end
function Polygon(points::Vector{Point{T}}) where {T}
    lines = Line{T}[]
    p0 = points[0]
    for p1 in points[2:end]
        push!(lines, Line(p0.x, p0.y, p1.x, p1.y))
        p0 = p1
    end
    return Polygon(lines)
end

transform(p::Point, x, y, θ) = Point(x + cosd(θ)*p.x - sind(θ)*p.y, y + sind(θ)*p.x + cosd(θ)*p.y)
transform(c::Circle, x, y, θ) = Circle(x + cosd(θ)*c.x - sind(θ)*c.y, y + sind(θ)*c.x + cosd(θ)*c.y, c.r)
transform(l::Line, x, y, θ) = Line(x + cosd(θ)*l.x1 - sind(θ)*l.y1, y + sind(θ)*l.x1 + cosd(θ)*l.y1, x + cosd(θ)*l.x2 - sind(θ)*l.y2, y + sind(θ)*l.x2 + cosd(θ)*l.y2)
transform(p::Polygon, x, y, θ) = Polygon(transform.(p.lines, x, y, θ))

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

    a1 == b1 == 0 && return 0 <= d2 <= 1
    a2 == b2 == 0 && return 0 <= d1 <= 1

    return d1 === d2 && 0 <= d1 <= 1 || 0 <= d2 <= 1
end

function intersects(l1::Line, l2::Line)
    # From https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    t = (l1.x1 - l2.x1)*(l2.y1 - l2.y2) - (l1.y1 - l2.y1)*(l2.x1 - l2.x2)
    d = (l1.x1 - l1.x2)*(l2.y1 - l2.y2) - (l1.y1 - l1.y2)*(l2.x1 - l2.x2)
    u = (l1.y1 - l1.y2)*(l1.x1 - l2.x1) - (l1.x1 - l1.x2)*(l1.y1 - l2.y1)

    # Parallel lines
    if iszero(t) && iszero(u) && iszero(d)
        return intersects(l1, Point(l2.x1, l2.y1)) || intersects(l1, Point(l2.x2, l2.y2))
    end

    return 0 <= t/d <= 1 && 0 <= u/d <= 1
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

    # 0 <= d1 <= 1 && println((l.x1*(1 - d1) + l.x2*d1, l.y1*(1 - d1) + l.y2*d1))
    # 0 <= d2 <= 1 && println((l.x1*(1 - d2) + l.x2*d2, l.y1*(1 - d2) + l.y2*d2))

    return 0 <= d1 <= 1 || 0 <= d2 <= 1
end
