module Shapes

using IterTools: partition
using LinearAlgebra

export Point, Line, circle, polygon

const flatten = Iterators.flatten

const SHAPE_COMPLEXITY_CUTOFF = 30

##################################################
# Basic shapes
##################################################

abstract type AbstractPrimitiveShape{T} end

struct Point{T} <: AbstractPrimitiveShape{T}
    x::T
    y::T
end
Point{T}((x, y)) where {T} = Point{T}(convert(T, x), convert(T, y))
Point(x, y) = Point(promote(x, y)...)
Point((x, y)) = Point(promote(x, y)...)
Base.convert(::Type{Point{T}}, p::Point{S}) where {T,S} = Point(convert(T, p.x), convert(T, p.y))
Base.promote_rule(::Type{Point{T}}, ::Type{Point{S}}) where {T,S} = Point{promote_type(T, S)}

struct Line{T} <: AbstractPrimitiveShape{T}
    p0::Point{T}
    p1::Point{T}
end

Base.convert(::Type{Line{T}}, l::Line{S}) where {T,S} = Line(convert(Point{T}, l.p0), convert(Point{T}, l.p1))

##################################################
# Non–basic shapes
##################################################

const NonPrimitiveShape{T} = Union{AbstractVector{<:AbstractPrimitiveShape{T}}, AbstractSet{<:AbstractPrimitiveShape{T}}, NTuple{<:Any, <:AbstractPrimitiveShape{T}}}
const Points{T} = Union{AbstractVector{<:Point{T}}, AbstractSet{<:Point{T}}, NTuple{<:Any, <:Point{T}}}
const Lines{T} = Union{AbstractVector{<:Line{T}}, AbstractSet{<:Line{T}}, NTuple{<:Any, <:Line{T}}}

polyline(points) = [Line(p0, p1) for (p0, p1) in partition(points, 2, 1)]
polygon(points) = polyline(flatten((points, first(points))))
circle(c::Point, r::Real; samples=20) = polyline(Point(c.x + r*cos(2π*i/samples), c.y + r*sin(2π*i/samples)) for i in 0:samples)

const AbstractShape{T} = Union{AbstractPrimitiveShape{T}, NonPrimitiveShape{T}}

Base.iterate(s::AbstractPrimitiveShape) = (s, nothing)
Base.iterate(::AbstractPrimitiveShape, ::Nothing) = nothing
Base.length(::AbstractPrimitiveShape) = 1
Base.getindex(s::AbstractPrimitiveShape, x) = (s,)[x]

##################################################
# BoundingBox
##################################################

struct BoundingBox{T}
    x1::T
    x2::T
    y1::T
    y2::T
end

BoundingBox(b::BoundingBox) = b
BoundingBox(b1::BoundingBox, b2) = BoundingBox(
    min(b1.x1, b2.x1), max(b1.x2, b2.x2),
    min(b1.y1, b2.y1), max(b1.y2, b2.y2))

Base.extrema(p::Point) = (p.x, p.x, p.y, p.y)
Base.extrema(l::Line) = (min(l.p0.x, l.p1.x), max(l.p0.x, l.p1.x), min(l.p0.y, l.p1.y), max(l.p0.y, l.p1.y))

BoundingBox(p::Point) = BoundingBox(extrema(p)...)
BoundingBox(l::Line) = BoundingBox(extrema(l)...)
BoundingBox(points::Points) = BoundingBox(extrema(p.x for p in points)..., extrema(p.y for p in points)...)
function BoundingBox(s::NonPrimitiveShape)
    x1, x2, y1, y2 = Inf, -Inf, Inf, -Inf
    for ps in s
        a, b, c, d = extrema(ps)
        a < x1 && (x1 = a)
        b > x2 && (x2 = b)
        c < y1 && (y1 = c)
        d > y2 && (y2 = d)
    end
    return BoundingBox(x1, x2, y1, y2)
end

intersects(b1::BoundingBox, b2::BoundingBox) = b1.x2 > b2.x1 && b2.x2 > b1.x1 && b1.y2 > b2.y1 && b2.y2 > b1.y1

##################################################
# Basic algebra
##################################################

Base.zero(::Point{T}) where {T} = Point(zero(T), zero(T))
Base.zero(::Type{Point{T}}) where {T} = Point(zero(T), zero(T))
Base.:-(p::Point) = Point(-p.x, -p.y)
Base.:+(p1::Point, p2::Point) = Point(p1.x + p2.x, p1.y + p2.y)
Base.:-(p1::Point, p2::Point) = Point(p1.x - p2.x, p1.y - p2.y)
Base.:*(p::Point, n::Real) = Point(p1.x*n, p1.y*n)
Base.:/(p::Point, n::Real) = Point(p1.x/n, p1.y/n)
LinearAlgebra.cross(p1::Point, p2::Point) = p1.x*p2.y - p2.x*p1.y
LinearAlgebra.dot(p1::Point, p2::Point) = p1.x*p2.x + p1.y*p2.y

##################################################
# Helper functions
##################################################

"""
    strongmul(x, y)

Same as `x*y` except zeros are treated as "strong", i. e.,
`strongmul(0.0, Inf) == 0.0`.

[[ Note: This function appears to have no loss of performance,
compared to normal floating point multiplication. ]]

"""
function strongmul(x, y)
    iszero(x) && isinf(y) && return x
    iszero(y) && isinf(x) && return y
    return x*y
end

# `⋖ (\lessdot)`; We need this custom operator to handle line tracing
# with infinite lines.
⋖(x, y) = (x == y == Inf || x == y == -Inf) ? x < y : x <= y

##################################################

"""
    vertices(shape)
"""
vertices(p::Point) = (p,)
vertices(l::Line) = (l.p0, l.p1)
vertices(s::NonPrimitiveShape) = flatten(map(vertices, s))

"""
    edges(shape)
"""
edges(p::Point) = ()
edges(l::Line) = (l,)
edges(s::NonPrimitiveShape) = flatten(map(edges, s))

struct Transformation{T}
    c::T
    s::T
    x::T
    y::T
end
Transformation(c, s, x, y) = Transformation(promote(c, s, x, y)...)

(t1::Transformation)(t2::Transformation) = Transformation(
    t1.c*t2.c - t1.s*t2.s,
    t1.c*t2.s + t1.s*t2.c,
    t1.x + t1.c*t2.x - t1.s*t2.y,
    t1.y + t1.s*t2.x + t1.c*t2.y)
(t::Transformation)(p::Point) = Point(t.x + t.c*p.x- t.s*p.y, t.y + t.c*p.y + t.s*p.x)
(t::Transformation)(l::Line) = Line(t(l.p0), t(l.p1))
(t::Transformation)(s::NonPrimitiveShape) = map(t, s)

"""
    translate(x, y)
    translate(shape, x, y)
"""
translate(x, y) = translate(promote(x, y)...)
translate(x::T, y::T) where T = Transformation(one(T), zero(T), x, y)
translate(s::AbstractShape, x, y) = translate(x, y)(s)

"""
    rotate(θ)
    rotate(shape, θ)
"""
rotate(θ) = Transformation(cosd(θ), sind(θ), 0., 0.)
rotate(s::AbstractShape, θ) = rotate(θ)(s)

"""
    extrude(transforms...)
"""
extrude(transform) = s -> extrude(s, transform)

"""
    extrude(shape, transforms...)
"""
extrude(p::Point, transform) = Line(p, transform(p))
extrude(ps::Points, transform) = map(extrude(transform), ps)
extrude(s::Union{Line, NonPrimitiveShape}, transform) =
    reduce(vcat, (s, transform(s), extrude(collect(vertices(s)), transform)))

"""
    trace(shape, line)

Return an unsorted vector of parameters, corresponding to each
intersection of `line` with `shape`.

"""
function trace(p::Point, l::Line)
    d = (l.p1 - l.p0) × (p - l.p0)
    # If `d` is zero, the point is in the line.
    if iszero(d)
        # We project `p` onto `l`,
        a = ((p - l.p0) ⋅ (l.p1 - l.p0)) / ((l.p1 - l.p0) ⋅ (l.p1 - l.p0))
        # and check if it is outside the bounds:
        (a < 0 || 1 < a || isnan(a)) && return ()
        # otherwise, we have a match!
        return (a,)
    end
    return ()
end

function trace(l1::Line, l2::Line)
    d = (l1.p1 - l1.p0) × (l2.p1 - l2.p0)
    # `t1/d` and `t2/d` are the parameters for the intersection point
    # using `l1` or `l2` respectively.  `t2` is defined a little later.
    t1 = (l2.p0 - l1.p0) × (l2.p1 - l2.p0)

    # For parallel (or anti–parallel) lines, `d` is zero.
    if iszero(d)
        # If `t1` (or `t2`) is also zero, then the two lines line up.
        if iszero(t1)
            # In this case, we trace the vertices of `l1` by
            # projecting them onto `l2`:
            v1, v2 = vertices(l1)
            a1 = ((v1 - l2.p0) ⋅ (l2.p1 - l2.p0)) / ((l2.p1 - l2.p0) ⋅ (l2.p1 - l2.p0))
            a2 = ((v2 - l2.p0) ⋅ (l2.p1 - l2.p0)) / ((l2.p1 - l2.p0) ⋅ (l2.p1 - l2.p0))
            amin, amax = min(a1, a2), max(a1, a2)
            # In the 1D projection, we check if there is no overlap,
            (amax < 0 || amin > 1 || isnan(amin) || isnan(amax)) && return ()
            # in which case we return the parameters corresponding to
            # the line segment of the overlap:
            return (max(amin, 0), min(amax, 1))
        end
        # If the lines do not line up, then they do not intersect.
        return ()
    end

    t2 = (l2.p0 - l1.p0) × (l1.p1 - l1.p0)

    # For non–parallel lines, we check if the parameter `t1/d` is in
    # `l1`, and return `t2` if it is on `l2` (TODO: include simple
    # proof why this works).  See above for the definition of the
    # operator `⋖` (`\lessdot`).
    if 0 <= t1/d <= 1 && 0 <= t2/d <= 1
        return (t2/d,)
    end
    return ()
end

trace(l::Line) = s -> trace(s, l)
trace(s::NonPrimitiveShape, l::Line) = flatten(map(trace(l), s))

"""
    trace(shape, lines)

Like `trace(shape, ::Line)`, except this will return the parameters
for all lines, combined.

This function is mostly useful when used in combination with
`extrude(vertices(...))` to find the distances between shapes along an
axis.

"""
function trace(s::AbstractShape, ls::Lines)
    !intersects(BoundingBox(s), BoundingBox(ls)) && return ()
    return flatten(trace(s, l) for l in ls)
end

"""
    intersects(shape)
"""
intersects(s2::AbstractShape) = (s1) -> intersects(s1, s2)

"""
    intersects(shape1, shape2)
"""
intersects(p1::Point, p2::Point) = p1 == p2

# See `trace` above for detailed comments of the code.
function intersects(p::Point, l::Line)
    d = (l.p1 - l.p0) × (p - l.p0)
    if iszero(d)
        a = ((p - l.p0) ⋅ (l.p1 - l.p0)) / ((l.p1 - l.p0) ⋅ (l.p1 - l.p0))
        (a < 0 || a > 1) && return false
        return true
    end
    return false
end
intersects(l::Line, p::Point) = intersects(p, l)

# See `trace` above for detailed comments of the code.
function intersects(l1::Line, l2::Line)
    d = (l1.p1 - l1.p0) × (l2.p1 - l2.p0)
    t1 = (l2.p0 - l1.p0) × (l2.p1 - l2.p0)
    if iszero(d)
        if iszero(t1)
            v1, v2 = vertices(l1)
            a1 = ((v1 - l2.p0) ⋅ (l2.p1 - l2.p0)) / ((l2.p1 - l2.p0) ⋅ (l2.p1 - l2.p0))
            a2 = ((v2 - l2.p0) ⋅ (l2.p1 - l2.p0)) / ((l2.p1 - l2.p0) ⋅ (l2.p1 - l2.p0))
            amin, amax = min(a1, a2), max(a1, a2)
            (amax < 0 || 1 < amin) && return false
            return true
        end
        return false
    end
    t2 = (l2.p0 - l1.p0) × (l1.p1 - l1.p0)
    if 0 <= t1/d <= 1 && 0 <= t2/d <= 1
        return true
    end
    return false
end

intersects(s1::NonPrimitiveShape, s2::AbstractPrimitiveShape) =
    any(intersects(s2, ps) for ps in s1)
intersects(s2::AbstractPrimitiveShape, s1::NonPrimitiveShape) = intersects(s1, s2)

function intersects(s1::NonPrimitiveShape, s2::NonPrimitiveShape)
    !intersects(BoundingBox(s1), BoundingBox(s2)) && return false
    return any(intersects(ps1, ps2) for ps1 in s1, ps2 in s2)
end

"""
    inside(shape1, shape2, θ)

Determine if `shape2` is inside `shape1`, using a rays from each point
at an angle `θ`.  A shape is determined to be inside if either:

 * the two shapes intersect, or;

 * a ray starting at one of the vertices of `shape2` intersects
   `shape1` an odd number of times.

"""
inside(p1::Point, p2::Point, θ) = intersects(p1, Line(Point(p.x, p.y), Point(p.x - cosd(θ)*1e6, p.y - sind(θ)*1e6)))
inside(l::Line, p::Point, θ) = intersects(l, Line(Point(p.x, p.y), Point(p.x - cosd(θ)*1e6, p.y - sind(θ)*1e6)))
inside(p::Point, l::Line, θ) = inside(l, p, θ)
inside(s1::AbstractShape, s2::AbstractShape, θ) = intersects(s1, s2) || any(p -> inside(s1, p, θ), vertices(s2)) || any(p -> inside(s2, p, θ), vertices(s1))

end #module
