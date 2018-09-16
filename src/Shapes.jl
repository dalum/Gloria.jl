module Shapes

using IterTools: partition
using LinearAlgebra

export Point, Line,
    circle, extrude, intersects, polygon, rotate, trace, transform, translate, vertices

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
    tmin::T
    tmax::T
end
Line(p0::Point{T}, p1::Point{T}, tmin = convert(T, 0), tmax = convert(T, 1)) where {T} = Line{T}(p0, p1, tmin, tmax)
# Line(x1, y1, x2, y2, tmin, tmax) = Line(Point(promote(x1, y1)...), Point(promote(x2, y2)...)..., tmin, tmax)
Line(x, y, θ; tmin = -Inf, tmax = Inf) = Line(Point(x, y), Point(x + cosd(θ), y + sind(θ)), tmin, tmax)
Base.convert(::Type{Line{T}}, l::Line{S}) where {T,S} = Line(convert(Point{T}, l.p0), convert(Point{T}, l.p1), convert(T, l.tmin), convert(T, l.tmax))

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
# Windowing
##################################################

struct Windowing{T}
    x1::T
    x2::T
    y1::T
    y2::T
end

Windowing(p::Point) = Windowing(p.x, p.x, p.y, p.y)
Windowing(l::Line) = Windowing(
    extrema((l.p0.x + (l.p1.x - l.p0.x)*l.tmin, l.p0.x + (l.p1.x - l.p0.x)*l.tmax))...,
    extrema((l.p0.y + (l.p1.y - l.p0.y)*l.tmin, l.p0.y + (l.p1.y - l.p0.y)*l.tmax))...)
Windowing(points::Points) = Windowing(extrema(p.x for p in points)..., extrema(p.y for p in points)...)
Windowing(s::NonPrimitiveShape) = Windowing(vertices(s))

intersects(w1::Windowing, w2::Windowing) = w1.x2 > w2.x1 && w2.x2 > w1.x1 && w1.y2 > w2.y1 && w2.y2 > w1.y1

##################################################
# Basic algebra
##################################################

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

# `⋖ (\lessdot)`; We need this custom operator to handle parallel
# lines and point-line intersection with infinite lines.
⋖(x, y) = (x == y == Inf || x == y == -Inf) ? x < y : x <= y

##################################################

"""
    vertices(shape)
"""
vertices(p::Point) = [p]
vertices(l::Line) = [Point(l.p0.x + strongmul(l.p1.x - l.p0.x, l.tmin), l.p0.y + strongmul(l.p1.y - l.p0.y, l.tmin)),
                     Point(l.p0.x + strongmul(l.p1.x - l.p0.x, l.tmax), l.p0.y + strongmul(l.p1.y - l.p0.y, l.tmax))]
vertices(s::NonPrimitiveShape) = unique!(reduce(vcat, vertices.(s)))

"""
    translate(shape, x, y)
"""
translate(x, y) = (s::AbstractShape) -> translate(s, x, y)
translate(p::Point, x, y) = Point(p.x + x, p.y + y)
translate(l::Line, x, y) = Line(translate(l.p0, x, y), translate(l.p1, x, y), l.tmin, l.tmax)
translate(s::NonPrimitiveShape, x, y) = translate.(s, x, y)

"""
    rotate(shape, x, y)
"""
rotate(θ) = (s::AbstractShape) -> rotate(s, θ)
rotate(p::Point, θ) = Point(p.x*cosd(θ) - p.y*sind(θ), p.x*sind(θ) + p.y*cosd(θ))
rotate(l::Line, θ) = Line(rotate(l.p0, θ), rotate(l.p1, θ), l.tmin, l.tmax)
rotate(s::NonPrimitiveShape, θ) = rotate.(s, θ)

"""
    transform(transforms...)
"""
transform(transforms...) = (s) -> transform(s, transforms...)

"""
    transform(shape, transforms...)
"""
@inline transform(s::AbstractShape, transforms...) = foldl(|>, transforms, init=s)

"""
    extrude(transforms...)
"""
extrude(transforms...) = (s) -> extrude(s, transforms...)

"""
    extrude(shape, transforms...)
"""
extrude(p::Point, transforms...) = Line(p, transform(p, transforms...))
extrude(ps::Points, transforms...) = [extrude(p, transforms...) for p in ps]
# extrude(l::Line, transforms...) =
#     [l, transform(l1, transforms...), extrude(vertices(l), transforms...)...]
extrude(s::Union{Line, NonPrimitiveShape}, transforms...) =
    reduce(vcat, (s, transform(s, transforms...), extrude(vertices(s), transforms...)))

"""
    trace(shape, line)

Return an unsorted vector of parameters, corresponding to each
intersection of `line` with `shape`.

"""
function trace(p::Point, l::Line)
    d = (l.p1 - l.p0) × p
    # If `d` is zero, the point is in the line.
    if iszero(d)
        # We project `p` onto `l`,
        a = ((p - l.p0) ⋅ (l.p1 - l.p0)) / ((l.p1 - l.p0) ⋅ (l.p1 - l.p0))
        # and check if it is outside the bounds:
        (a < l.tmin || a > l.tmax) && return Float64[]
        # otherwise, we have a match!
        return Float64[a]
    end
    return Float64[]
end

function trace(l1::Line, l2::Line)
    d = (l1.p1 - l1.p0) × (l2.p1 - l2.p0)
    # `t1/d` and `t2/d` are the parameters for the intersection point
    # using `l1` or `l2` respectively.
    t1 = (l2.p0 - l1.p0) × (l2.p1 - l2.p0)
    t2 = (l2.p0 - l1.p0) × (l1.p1 - l1.p0)

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
            (amax < l2.tmin || l2.tmax < amin) && return Float64[]
            # in which case we return the parameters corresponding to
            # the line segment of the overlap:
            return Float64[max(amin, l2.tmin), min(amax, l2.tmax)]
        end
        # If the lines do not line up, then they do not intersect.
        return Float64[]
    end

    # For non–parallel lines, we check if the parameter `t1/d` is in
    # `l1`, and return `t2` if it is on `l2` (TODO: include simple
    # proof why this works).  See above for the definition of the
    # operator `⋖` (`\lessdot`).
    if l1.tmin ⋖ t1/d ⋖ l1.tmax && l2.tmin ⋖ t2/d ⋖ l2.tmax
        return Float64[t2/d]
    end
    return Float64[]
end

trace(s::NonPrimitiveShape, l::Line) = reduce(vcat, (trace(ps, l) for ps in s))

"""
    trace(shape, lines)

Like `trace(shape, ::Line)`, except this will return the parameters
for all lines, combined.

This function is mostly useful when used in combination with
`extrude(vertices(...))` to find the distances between shapes along an
axis.

"""
function trace(s::AbstractShape, ls::Lines)
    if length(s) > SHAPE_COMPLEXITY_CUTOFF || length(ls) > SHAPE_COMPLEXITY_CUTOFF
        return _trace_complex(s, ls)
    end
    return reduce(vcat, [trace(s, l) for l in ls])
end

function _trace_complex(s::AbstractShape, ls::Lines; n = SHAPE_COMPLEXITY_CUTOFF)
    # TODO: improve performance
    ts = Float64[]
    for i in 1:n:length(s), j in 1:n:length(ls)
        ss = s[i:min(i+n-1, length(s))]
        lss = ls[j:min(j+n-1, length(ls))]
        # If the windowings do not intersect, neither can the shapes
        !intersects(Windowing(ss), Windowing(lss)) && continue
        append!(ts, reduce(vcat, [trace(ss, l) for l in lss]))
    end
    return ts
end

"""
    intersects(shape)
"""
intersects(s2::AbstractShape) = (s1) -> intersects(s1, s2)

"""
    intersects(shape1, shape2)
"""
intersects(p1::Point, p2::Point) = p1 == p2
intersects(p::Point, l::Line) = length(trace(p, l)) > 0
intersects(l::Line, p::Point) = intersects(p, l)
intersects(l1::Line, l2::Line) = length(trace(l1, l2)) > 0
intersects(s1::NonPrimitiveShape, s2::AbstractPrimitiveShape) =
    any(intersects(s2, ps) for ps in s1)
intersects(s2::AbstractPrimitiveShape, s1::NonPrimitiveShape) = intersects(s1, s2)

function intersects(s1::NonPrimitiveShape, s2::NonPrimitiveShape)
    if length(s1) > SHAPE_COMPLEXITY_CUTOFF || length(s2) > SHAPE_COMPLEXITY_CUTOFF
        return _intersects_complex(s1, s2)
    end
    return any(intersects(ps1, ps2) for ps1 in s1, ps2 in s2) && return true
end

function _intersects_complex(s1::NonPrimitiveShape, s2::NonPrimitiveShape; n::Integer = SHAPE_COMPLEXITY_CUTOFF)
    for i in 1:n:length(s1), j in 1:n:length(s2)
        ss1 = s1[i:min(i+n-1, length(s1))]
        ss2 = s2[j:min(j+n-1, length(s2))]
        # If the windowings do not intersect, neither can the shapes
        !intersects(Windowing(ss1), Windowing(ss2)) && continue
        any(intersects(ps1, ps2) for ps1 in ss1, ps2 in ss2) && return true
    end
    return false
end

"""
    inside(shape1, shape2, θ)

Determine if `shape2` is inside `shape1`, using a rays from each point
at an angle `θ`.  A shape is determined to be inside if either:

 * the two shapes intersect, or;

 * a ray starting at one of the vertices of `shape2` intersects
   `shape1` an odd number of times.

"""
inside(p1::Point, p2::Point, θ) = intersects(p1, Line(p2.x, p2.y, θ; tmin=-Inf, tmax=0.))
inside(l::Line, p::Point, θ) = intersects(l, Line(p.x, p.y, θ; tmin=-Inf, tmax=0.))
inside(p::Point, l::Line, θ) = inside(l, p, θ)
inside(l1::Line, l2::Line, θ) = intersects(l1, l2) || any(p->inside(l1, p, θ), vertices(l2)) || any(p->inside(l2, p, θ), vertices(l1))
inside(s::NonPrimitiveShape, l::Line, θ) = intersects(l1, l2) || any(p->inside(l1, p, θ), vertices(l2)) || any(p->inside(l2, p, θ), vertices(l1))

end #module
