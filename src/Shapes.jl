module Shapes

using IterTools: partition
using LinearAlgebra
using StaticArrays

export AbstractShape, Point, Polyline,
    circle, edges, polygon, rotate, translate, vertices

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

##################################################
# Non–basic shapes
##################################################

const Polyline{N,T} = NTuple{N, <:Point{T}}

polygon(points) = Tuple(flatten((points, first(points))))
circle(c::Point, r::Real; samples=20, θ=0.) = Tuple(Point(c.x + r*cosd(360*i/samples + θ), c.y + r*sind(360*i/samples + θ)) for i in 0:samples)

const NonPrimitiveShape{T} = Union{Polyline{T}}
const AbstractShape{T} = Union{AbstractPrimitiveShape{T}, NonPrimitiveShape{T}}

Base.iterate(s::AbstractPrimitiveShape) = (s, nothing)
Base.iterate(::AbstractPrimitiveShape, ::Nothing) = nothing
Base.length(::AbstractPrimitiveShape) = 1
Base.getindex(s::AbstractPrimitiveShape, x) = (s,)[x]

ispolygon(s::AbstractShape) = first(s) === last(s)

function subdivide(s::Polyline, samples=2)
    points = flatten((p1 + i*(p2 - p1) / samples for i in 0:samples-1) for (p1, p2) in edges(s))
    return Tuple(flatten((points, s[end])))
end

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
BoundingBox(p::Point) = BoundingBox(p.x, p.x, p.y, p.y)
function BoundingBox(s::Polyline)
    x1, x2, y1, y2 = Inf, -Inf, Inf, -Inf
    for p in s
        p.x < x1 && (x1 = p.x)
        p.x > x2 && (x2 = p.x)
        p.y < y1 && (y1 = p.y)
        p.y > y2 && (y2 = p.y)
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
Base.:*(p::Point, n::Real) = Point(p.x*n, p.y*n)
Base.:*(n::Real, p::Point) = Point(p.x*n, p.y*n)
Base.:/(p::Point, n::Real) = Point(p.x/n, p.y/n)
LinearAlgebra.cross(p1::Point, p2::Point) = p1.x*p2.y - p2.x*p1.y
LinearAlgebra.cross(n::Float64, p::Point) = Point(-n*p.y, p.x*n)
LinearAlgebra.cross(p::Point, n::Float64) = Point(n*p.y, -p.x*n)
LinearAlgebra.dot(p1::Point, p2::Point) = p1.x*p2.x + p1.y*p2.y
LinearAlgebra.norm(p::Point) = sqrt(p.x*p.x + p.y*p.y)
LinearAlgebra.normalize(p::Point) = (N = norm(p); Point(p.x/N, p.y/N))
vector(l::Polyline{2}) = l[2] - l[1]

Base.isapprox(p1::Point, p2::Point) = isapprox(p1.x, p2.x) && isapprox(p1.y, p2.y)

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
vertices(l::Polyline) = ispolygon(l) ? l[1:end-1] : l

"""
    edges(shape)
"""
edges(p::Point) = ()
edges(l::Polyline) = partition(l, 2, 1)

##################################################
# Distances
##################################################

"""
    projection(line, point)

Return the projection of `point` onto `line`.  If the `point` is not
normal to the `line`, return `nothing` instead.

"""
function projection(l::Polyline{2}, p::Point)
    @inbounds d = (l[2] - l[1])
    @inbounds a = (d ⋅ (p - l[1])) / (d ⋅ d)
    0 <= a <= 1 && return @inbounds l[1] + d * a
    return nothing
end

closestprojection(l::Polyline{2}, p::Point) = projection(l, p)
function closestprojection(l::Polyline, p::Point)
    d = Inf
    pr = p
    for e in edges(l)
        pr′ = projection(e, p)
        if pr′ !== nothing
            d′ = norm(p - pr′)
            if d′ < d
                d = d′
                pr = pr′
            end
        end
    end
    return pr
end

##################################################
# Transformations
##################################################

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
(t::Transformation)(l::Polyline) = map(t, l)

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
extrude(p::Point, transform) = (p, transform(p))
extrude(l::Polyline{2}, transform) = Tuple(flatten((l, reverse(transform(l)), l[1])))

"""
    trace(shape, line)

Return an unsorted vector of parameters, corresponding to each
intersection of `line` with `shape`.

"""
trace(s::Polyline{1}, l::Polyline{2}) = trace(s[1], l)
function trace(p::Point, l::Polyline{2})
    @inbounds b = (l[2] - l[1])
    @inbounds c = (p - l[1])
    # If `d` is zero, the point is in the line.
    if iszero(b × c)
        # We project `p` onto `l`,
        @inbounds a = (c ⋅ b) / (b ⋅ b)
        # and check if it is outside the bounds:
        (a <= 0 || 1 <= a || isnan(a)) && return ()
        # otherwise, we have a match!
        return (a,)
    end
    return ()
end

function trace(l1::Polyline{2}, l2::Polyline{2})
    @inbounds b = (l1[2] - l1[1])
    @inbounds c = (l2[2] - l2[1])
    @inbounds k = (l2[1] - l1[1])
    d = b × c
    # `t1/d` and `t2/d` are the parameters for the intersection point
    # using `l1` or `l2` respectively.  `t2` is defined a little later.
    t1 = k × c

    # For parallel (or anti–parallel) lines, `d` is zero.
    if iszero(d)
        # If `t1` (or `t2`) is also zero, then the two lines line up.
        if iszero(t1)
            # In this case, we trace the vertices of `l1` by
            # projecting them onto `l2`:
            @inbounds v1, v2 = vertices(l1)
            n = (c ⋅ c)
            @inbounds a1 = ((v1 - l2[1]) ⋅ c) / n
            @inbounds a2 = ((v2 - l2[1]) ⋅ c) / n
            amin, amax = min(a1, a2), max(a1, a2)
            # In the 1D projection, we check if there is no overlap,
            (amax <= 0 || 1 <= amin || isnan(amin) || isnan(amax)) && return ()
            # in which case we return the parameters corresponding to
            # the interval of  overlap of the line segments:
            return (max(amin, 0.), min(amax, 1.))
        end
        # If the lines do not line up, then they do not intersect.
        return ()
    end

    t2 = k × b

    # For non–parallel lines, we check if the parameter `t1/d` is in
    # `l1`, and return `t2` if it is on `l2` (TODO: include simple
    # proof why this works).
    if 0 <= t1/d <= 1 && 0 <= t2/d <= 1
        return (t2/d,)
    end
    return ()
end

trace(s::Polyline, l::Polyline{2}) = flatten(map(e -> trace(e, l), edges(s)))

"""
    intersects(shape1, shape2)
"""
intersects(p1::Point, p2::Point) = p1 == p2

# See `trace` above for detailed comments of the code.
function intersects(p::Point, l::Polyline{2})
    @inbounds b = (l[2] - l[1])
    @inbounds c = (p - l[1])
    if iszero(b × c)
        @inbounds a = (c ⋅ b) / (b ⋅ b)
        (a <= 0 || 1 <= a || isnan(a)) && return false
        return true
    end
    return false
end
intersects(l::Polyline{2}, p::Point) = intersects(p, l)

# See `trace` above for detailed comments of the code.
function intersects(l1::Polyline{2}, l2::Polyline{2})
    @inbounds b = (l1[2] - l1[1])
    @inbounds c = (l2[2] - l2[1])
    @inbounds k = (l2[1] - l1[1])
    d = b × c
    t1 = k × c

    if iszero(d)
        if iszero(t1)
            @inbounds v1, v2 = vertices(l1)
            n = (c ⋅ c)
            @inbounds a1 = ((v1 - l2[1]) ⋅ c) / n
            @inbounds a2 = ((v2 - l2[1]) ⋅ c) / n
            amin, amax = min(a1, a2), max(a1, a2)
            (amax < 0 || 1 <= amin || isnan(amin) || isnan(amax)) && return false
            return true
        end
        return false
    end
    t2 = k × b
    if 0 <= t1/d <= 1 && 0 <= t2/d <= 1
        return true
    end
    return false
end

intersects(s1::AbstractShape, s2::Union{Point, Polyline{1}, Polyline{2}}) = any(intersects(e, s2) for e in edges(s1))
intersects(s1::Union{Point, Polyline{1}, Polyline{2}}, s2::AbstractShape) = intersects(s2, s1)

function intersects(s1::AbstractShape, s2::AbstractShape)
    !intersects(BoundingBox(s1), BoundingBox(s2)) && return false
    return any(intersects(e1, e2) for e1 in edges(s1), e2 in edges(s2))
end

"""
    inside(shape1, shape2, θ)

Determine if `shape2` is inside `shape1`, using a rays from each point
at an angle `θ`.  A shape is determined to be inside if either:

 * the two shapes intersect, or;

 * a ray starting at one of the vertices of `shape2` intersects
   `shape1` an odd number of times.

"""
inside(p1::Point, p2::Point, θ) = intersects(p1, (p2, p2 + Point(cosd(θ)*1e6, sind(θ)*1e6)))
function inside(l::Polyline, p::Point, θ)
    ray = (p, p + Point(cosd(θ)*1e6, sind(θ)*1e6))
    n = count(edges(l)) do e
        if intersects(e, ray)
            i1 = intersects(e[1], ray)
            i2 = intersects(e[2], ray)
            # Intersecting both end points counts as a double
            # intersection.
            i1 && i2 && return false
            # Not intersecting either end point is a valid
            # intersection.
            !(i1 || i2) && return true
            # Intersecting one vertex only counts if the the other
            # vertex of the edge lies clockwise w.r.t. to the ray.
            # This overcomes the issue of tracing corners
            d = ray[2] - ray[1]
            (i1 && d × (e[2] - ray[1]) >= 0) && return true
            (i2 && d × (e[1] - ray[1]) >= 0) && return true
        end
        return false
    end
    return isodd(n)
end

end #module
