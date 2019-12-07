module Shapes

using IterTools: partition
using LinearAlgebra
using StaticArrays: SVector, SMatrix

export AbstractShape, AbstractLineShape,
    HalfPlane, HalfSpace, Point, Polygon, Polyline, Vertex,
    circle, edges, polygon, rotate, translate, vertices

const flatten = Iterators.flatten

# """
#     Vector{D,T}

# A geometric vector in `D` dimensions of data type `T`.

# """
# struct Vector{D,T}
#     data::NTuple{D,T}
# end
# Vector(coordinates...) = Vector(promote(coordinates...))
# Vector(n::T) where {T<:Number} = Vector{1,T}((n,))
# Vector(iterable) = Vector(iterable...)
# Base.convert(::Type{Vector{D,T}}, p::Vector{D,S}) where {D,T,S} = Vector(convert(NTuple{D,T}, p.data))
# Base.promote_rule(::Type{Vector{D,T}}, ::Type{Vector{D,S}}) where {D,T,S} = Vector{D,promote_type(T, S)}

# @inline Base.getproperty(a::Vector, v::Symbol) = _getproperty(a, Val(v))
# @inline _getproperty(a::Vector, ::Val{v}) where {v} = Core.getfield(a, v)
# @inline _getproperty(a::Vector, ::Val{:x}) = Core.getfield(a, :data)[1]
# @inline _getproperty(a::Vector, ::Val{:y}) = Core.getfield(a, :data)[2]
# @inline _getproperty(a::Vector, ::Val{:z}) = Core.getfield(a, :data)[3]
# Base.iterate(a::Vector) = iterate(Core.getfield(a, :data))
# Base.iterate(a::Vector, n) = iterate(Core.getfield(a, :data), n)
# @inline Base.getindex(a::Vector, idx) = Base.getindex(Core.getfield(a, :data), idx)
# Base.length(a::Vector) = length(Core.getfield(a, :data))
# Base.lastindex(a::Vector) = lastindex(Core.getfield(a, :data))

##################################################
# Basic shapes
##################################################

abstract type AbstractShape{D,T} end

"""
    AbstractPrimitiveShape{D,T}

A primitive shape in `D` dimensions of data type `T`.

"""
abstract type AbstractPrimitiveShape{D,T} <: AbstractShape{D,T} end

"""
    AbstractLineShape{N,D,T}

A shape consisting of `N` vertices connected by line segments in `D`
dimensions of data type `T`.

"""
abstract type AbstractLineShape{N,D,T} <: AbstractShape{D,T} end

"""
    Vertex{D,T}

A vertex in `D` dimensions of data type `T`.

"""
struct Vertex{D,T} <: AbstractPrimitiveShape{D,T}
    data::NTuple{D,T}
end
Vertex(coordinates...) = Vertex(promote(coordinates...))
Vertex(n::T) where {T<:Number} = Vertex{1,T}((n,))
Vertex(iterable) = Vertex(iterable...)
Base.convert(::Type{Vertex{D,T}}, p::Vertex{D,S}) where {D,T,S} = Vertex(convert(NTuple{D,T}, p.data))
Base.promote_rule(::Type{Vertex{D,T}}, ::Type{Vertex{D,S}}) where {D,T,S} = Vertex{D,promote_type(T, S)}

@inline Base.getproperty(a::Vertex, v::Symbol) = _getproperty(a, Val(v))
@inline _getproperty(a::Vertex, ::Val{v}) where {v} = Core.getfield(a, v)
@inline _getproperty(a::Vertex, ::Val{:x}) = Core.getfield(a, :data)[1]
@inline _getproperty(a::Vertex, ::Val{:y}) = Core.getfield(a, :data)[2]
@inline _getproperty(a::Vertex, ::Val{:z}) = Core.getfield(a, :data)[3]
Base.iterate(a::Vertex) = iterate(Core.getfield(a, :data))
Base.iterate(a::Vertex, n) = iterate(Core.getfield(a, :data), n)
@inline Base.getindex(a::Vertex, idx) = Base.getindex(Core.getfield(a, :data), idx)
Base.length(a::Vertex) = length(Core.getfield(a, :data))
Base.lastindex(a::Vertex) = lastindex(Core.getfield(a, :data))

"""
    Polyline{N,D,T}

A collection of line segments connecting `N` vertices, in `D`
dimensions of data type `T`.

"""
struct Polyline{N,D,T} <: AbstractLineShape{N,D,T}
    data::NTuple{N,Vertex{D,T}}
end
Polyline(iterable) = Polyline(Tuple(iterable))
Polyline(vertices::Vertex{D,T}...) where {D,T} = Polyline(vertices)

const Point{D,T} = Polyline{1,D,T}
const LineSegment{D,T} = Polyline{2,D,T}

"""
    Polygon{N,D,T}

Similar to `Polyline`, except the last and first vertices are also
connected by a line segment.

"""
struct Polygon{N,D,T} <: AbstractLineShape{N,D,T}
    data::NTuple{N,Vertex{D,T}}
end
Polygon(iterable) = Polygon(Tuple(iterable))
Polygon(vertices::Vertex{D,T}...) where {D,T} = Polygon(vertices)

Base.iterate(a::AbstractLineShape) = iterate(a.data)
Base.iterate(a::AbstractLineShape, n) = iterate(a.data, n)
Base.getindex(a::AbstractLineShape, idx) = getindex(a.data, idx)
Base.length(a::AbstractLineShape) = length(a.data)
Base.lastindex(a::AbstractLineShape) = lastindex(a.data)

circle(c::Vertex{2}, r::Real; samples=20, θ=0.) = Polygon(Vertex(c.x + r*cosd(360*i/samples + θ), c.y + r*sind(360*i/samples + θ)) for i in 0:samples-1)

"""
    HalfSpace{D,T}

A shape that separates a space of `D` dimensions into two.

"""
struct HalfSpace{D,T} <: AbstractShape{D,T}
    vertex::Vertex{D,T}
    normal::SVector{D,T}
end

const HalfLine{T} = HalfSpace{1,T}
const HalfPlane{T} = HalfSpace{2,T}

##################################################
# BoundingBox
##################################################

function axes(::AbstractShape{D,T}) where {D,T}
    m = SMatrix{D,D}(Matrix(one(T)*I, D, D))
    return [m[:,i] for i in 1:D]
end

##################################################
# BoundingBox
##################################################

"""
    BoundingBox{D,T}

A bounding box in `D` dimensions of data type `T`.

"""
struct BoundingBox{D,T}
    data::NTuple{D,Pair{T,T}}
end
# BoundingBox(iterable) = BoundingBox(Tuple(iterable))
BoundingBox(bounds::Pair{T,T}...) where T = BoundingBox(bounds)

@inline Base.getproperty(a::BoundingBox, v::Symbol) = _getproperty(a, Val(v))
@inline _getproperty(a::BoundingBox, ::Val{v}) where v = Core.getfield(a, v)
@inline _getproperty(a::BoundingBox, ::Val{:x}) = Core.getfield(a, :data)[1]
@inline _getproperty(a::BoundingBox, ::Val{:y}) = Core.getfield(a, :data)[2]
@inline _getproperty(a::BoundingBox, ::Val{:z}) = Core.getfield(a, :data)[3]
Base.iterate(a::BoundingBox) = iterate(Core.getfield(a, :data))
Base.iterate(a::BoundingBox, n) = iterate(Core.getfield(a, :data), n)
Base.getindex(a::BoundingBox, idx) = getindex(Core.getfield(a, :data), idx)
Base.length(a::BoundingBox) = length(Core.getfield(a, :data))
Base.lastindex(a::BoundingBox) = lastindex(a.data)

BoundingBox(b::BoundingBox) = b
BoundingBox(a::BoundingBox{1}, b::BoundingBox{1}) = BoundingBox(
    min(first(a.x), first(b.x)) => max(last(a.x), last(b.x)))
BoundingBox(a::BoundingBox{2}, b::BoundingBox{2}) = BoundingBox(
    min(first(a.x), first(b.x)) => max(last(a.x), last(b.x)),
    min(first(a.y), first(b.y)) => max(last(a.y), last(b.y)))
BoundingBox(a::BoundingBox{3}, b::BoundingBox{3}) = BoundingBox(
    min(first(a.x), first(b.x)) => max(last(a.x), last(b.x)),
    min(first(a.y), first(b.y)) => max(last(a.y), last(b.y)),
    min(first(a.z), first(b.z)) => max(last(a.z), last(b.z)))
BoundingBox(a::BoundingBox{D}, b::BoundingBox{D}) where {D} = BoundingBox(
    min(first(ax), first(bx)) => max(last(ax), last(bx)) for (ax,bx) in zip(a, b))

BoundingBox(a::Vertex{2}) = BoundingBox(a.x => a.x, a.y => a.y)
BoundingBox(a::Vertex{3}) = BoundingBox(a.x => a.x, a.y => a.y, a.z => a.z)
BoundingBox(a::Vertex) = BoundingBox(x => x for x in a)

function BoundingBox(a::AbstractLineShape{N,1}) where {N}
    xmin, xmax = Inf, -Inf
    for v in vertices(a)
        v.x < xmin && (xmin = v.x)
        v.x > xmax && (xmax = v.x)
    end
    return BoundingBox(xmin => xmax)
end

function BoundingBox(a::AbstractLineShape{N,2}) where {N}
    xmin, xmax = Inf, -Inf
    ymin, ymax = Inf, -Inf
    for v in vertices(a)
        v.x < xmin && (xmin = v.x)
        v.x > xmax && (xmax = v.x)
        v.y < ymin && (ymin = v.y)
        v.y > ymax && (ymax = v.y)
    end
    return BoundingBox(xmin => xmax, ymin => ymax)
end

function BoundingBox(a::AbstractLineShape{N,3}) where {N}
    xmin, xmax = Inf, -Inf
    ymin, ymax = Inf, -Inf
    zmin, zmax = Inf, -Inf
    for v in vertices(a)
        v.x < xmin && (xmin = v.x)
        v.x > xmax && (xmax = v.x)
        v.y < ymin && (ymin = v.y)
        v.y > ymax && (ymax = v.y)
        v.z < zmin && (zmin = v.z)
        v.z > zmax && (zmax = v.z)
    end
    return BoundingBox(xmin => xmax, ymin => ymax, zmin => zmax)
end

function BoundingBox(a::AbstractLineShape{N,D}) where {N,D}
    mins = fill(Inf, D)
    maxs = fill(-Inf, D)
    for v in vertices(a), d in 1:D
        v[d] < mins[d] && (mins[d] = v[d])
        v[d] > maxs[d] && (maxs[d] = v[d])
    end
    return BoundingBox(map(Pair, mins, maxs)...)
end

function BoundingBox(s::HalfSpace{D,T}) where {D,T}
    mins = fill(Inf, D)
    maxs = fill(-Inf, D)
    axs = axes(s)
    for i in 1:D
        a = axs[i]
        if iszero(a × s.normal)
            x = a ⋅ vector(s.vertex)
            if a ⋅ s.normal >= 0
                mins[i] = -Inf
                maxs[i] = x
                continue
            end
            mins[i] = x
            maxs[i] = Inf
            continue
        end
        mins[i] = -Inf
        maxs[i] = Inf
    end
    return BoundingBox(map(Pair, mins, maxs)...)
end

intersects(a::BoundingBox{1}, b::BoundingBox{1}) =
    last(a.x) > first(b.x) && last(b.x) > first(a.x)
intersects(a::BoundingBox{2}, b::BoundingBox{2}) =
    last(a.x) > first(b.x) && last(b.x) > first(a.x) &&
    last(a.y) > first(b.y) && last(b.y) > first(a.y)
intersects(a::BoundingBox{3}, b::BoundingBox{3}) =
    last(a.x) > first(b.x) && last(b.x) > first(a.x) &&
    last(a.y) > first(b.y) && last(b.y) > first(a.y) &&
    last(a.z) > first(b.z) && last(b.z) > first(a.z)
intersects(a::BoundingBox{D}, b::BoundingBox{D}) where {D} = all(
    last(ax) > first(bx) && last(bx) > first(ax) for (ax,bx) in zip(a, b))

##################################################
# Basic algebra
##################################################

Base.isapprox(a::Vertex{1}, b::Vertex{1}) = isapprox(a.x, b.x)
Base.isapprox(a::Vertex{2}, b::Vertex{2}) = isapprox(a.x, b.x) && isapprox(a.y, b.y)
Base.isapprox(a::Vertex{3}, b::Vertex{3}) = isapprox(a.x, b.x) && isapprox(a.y, b.y) && isapprox(a.z, b.z)
Base.isapprox(a::Vertex{D}, b::Vertex{D}) where {D} = all(isapprox(ax, bx) for (ax,bx) in zip(a, b))

"""
    vector(a::Vertex)

Return a vector pointing from the origin to `a`.

"""
vector(a::Vertex{1}) = SVector(a.x)
vector(a::Vertex{2}) = SVector(a.x, a.y)
vector(a::Vertex{3}) = SVector(a.x, a.y, a.z)
vector(a::Vertex) = SVector(a.data...)

"""
    vector(a::Vertex, b::Vertex)

Return a vector pointing from `a` to `b`.

"""
vector(a::Vertex{1}, b::Vertex{1}) = SVector(b.x - a.x)
vector(a::Vertex{2}, b::Vertex{2}) = SVector(b.x - a.x, b.y - a.y)
vector(a::Vertex{3}, b::Vertex{3}) = SVector(b.x - a.x, b.y - a.y, b.z - a.z)
vector(a::Vertex{N}, b::Vertex{N}) where N = SVector(map(-, b, a)...)

"""
    vector(p::Point)

Return a vector pointing from the origin to `p`.

"""
vector(p::Point) = vector(first(p))

"""
    vector(l::LineSegment)

Return a vector pointing along the line segment `l`.

"""
vector(l::LineSegment) = vector(first(l), last(l))

"""
    vertices(shape)
"""
vertices(p::Vertex) = (p,)
vertices(l::AbstractLineShape) = l.data

"""
    edges(shape)
"""
edges(p::Vertex{D,T}) where {D,T} = Polyline{2,D,T}[]

function edges(l::Polyline{N,D,T}) where {N,D,T}
    result = Base.Vector{Polyline{2,D,T}}(undef, N - 1)
    for i in 1:N-1
        result[i] = Polyline(l[i], l[i+1])
    end
    return result
end

function edges(l::Polygon{N,D,T}) where {N,D,T}
    result = Base.Vector{Polyline{2,D,T}}(undef, N)
    for i in 1:N-1
        result[i] = Polyline(l[i], l[i+1])
    end
    result[N] = Polyline(l[N], l[1])
    return result
end

function edges(a::HalfSpace{2,T}) where {T}
    l = Polyline(a.vertex, a.vertex |> translate((a.normal |> rotate(90))...))
    return (Polyline(parameterized(l, -1e6), parameterized(l, 1e6)),)
end

"""
    normals(shape)
"""
function normals(l::Polyline{N,2,T}) where {N,T}
    result = Base.Vector{SVector{2,T}}(undef, N - 1)
    for i in 1:N-1
        result[i] = normalize(vector(l[i], l[i+1]) |> rotate(-90))
    end
    return result
end

function normals(l::Polygon{N,2,T}) where {N,T}
    result = Base.Vector{SVector{2,T}}(undef, N)
    for i in 1:N-1
        result[i] = normalize(vector(l[i], l[i+1]) |> rotate(-90))
    end
    result[N] = normalize(vector(l[N], l[1]) |> rotate(-90))
    return result
end

normals(a::HalfSpace{D,T}) where {D,T} = (a.normal,)

##################################################
# Distances
##################################################

"""
    parameterized(line, a)
"""
parameterized(l::LineSegment{1}, a) = Vertex(
    l[1].x + (l[2].x - l[1].x)*a)
parameterized(l::LineSegment{2}, a) = Vertex(
    l[1].x + (l[2].x - l[1].x)*a,
    l[1].y + (l[2].y - l[1].y)*a)
parameterized(l::LineSegment{3}, a) = Vertex(
    l[1].x + (l[2].x - l[1].x)*a,
    l[1].y + (l[2].y - l[1].y)*a,
    l[1].z + (l[2].z - l[1].z)*a)
parameterized(l::LineSegment{D}, a) where {D} = Vertex(
    l[1][i] + (l[2][i] - l[1][i])*a for i in 1:D)

parameterrange(::LineSegment{D,T}) where {D,T} = (T(0), T(1))

"""
    projection(point, line)

Return the projection of `point` onto `line`.  If the projected point
does not lie on `line`, return `nothing`.

"""
function projection(p::Vertex{D}, l::LineSegment{D}) where {D}
    amin, amax = parameterrange(l)
    d = vector(l)
    a = d ⋅ vector(l[1], p) / (d ⋅ d)
    amin <= a <= amax && return parameterized(l, a)
    return nothing
end

"""
    projection(shape, axis)

Return `shape` projected onto `axis`.

"""
function projection(s::AbstractShape{D}, a::SVector{D}) where {D}
    xmin, xmax = Inf, -Inf
    for v in vertices(s)
        x = a ⋅ vector(v)
        x < xmin && (xmin = x)
        x > xmax && (xmax = x)
    end
    return Polyline(Vertex(xmin), Vertex(xmax))
end

function projection(s::HalfSpace{D,T}, a::SVector{D,T}) where {D,T}
    if iszero(a × s.normal)
        x = a ⋅ vector(s.vertex)
        if a ⋅ s.normal >= 0
            return Polyline(Vertex(T(-Inf)), Vertex(x))
        end
        return Polyline(Vertex(x), Vertex(T(Inf)))
    end
    return Polyline(Vertex(T(-Inf)), Vertex(T(Inf)))
end

"""
    centroid(a, bs...)

Return the centroid (also known as the geometric center) of the
vertices of the shapes, `a, bs...` with equal weight given to each
vertex.

"""
function centroid(a::AbstractShape{1,T}) where {T}
    x = zero(T)
    N = length(vertices(a))
    for v in vertices(a)
        x += v.x
    end
    return Vertex(x/N)
end

function centroid(a::AbstractShape{2,T}) where {T}
    x, y = zero(T), zero(T)
    N = length(vertices(a))
    for v in vertices(a)
        x += v.x
        y += v.y
    end
    return Vertex(x/N, y/N)
end

function centroid(a::AbstractShape{3,T}) where {T}
    x, y, z = zero(T), zero(T), zero(T)
    N = length(vertices(a))
    for v in vertices(a)
        x += v.x
        y += v.y
        z += v.z
    end
    return Vertex(x/N, y/N, z/N)
end

function centroid(a::AbstractShape{D,T}) where {D,T}
    x = fill(zero(T), D)
    N = length(vertices(a))
    for v in vertices(a), i in 1:D
        x[i] += v[i]
    end
    for i in 1:D
        x[i] /= N
    end
    return Vertex(x)
end

function centroid(a::AbstractShape{1}, b::AbstractShape{1})
    aN, bN = length(vertices(a)), length(vertices(b))
    av, bv = centroid(a), centroid(b)
    N = aN + bN
    return Vertex((aN*av.x + bN*bv.x)/N)
end

function centroid(a::AbstractShape{2}, b::AbstractShape{2})
    aN, bN = length(vertices(a)), length(vertices(b))
    av, bv = centroid(a), centroid(b)
    N = aN + bN
    return Vertex((aN*av.x + bN*bv.x)/N, (aN*av.y + bN*bv.y)/N)
end

function centroid(a::AbstractShape{3}, b::AbstractShape{3})
    aN, bN = length(vertices(a)), length(vertices(b))
    av, bv = centroid(a), centroid(b)
    N = aN + bN
    return Vertex((aN*av.x + bN*bv.x)/N, (aN*av.y + bN*bv.y)/N, (aN*av.z + bN*bv.z)/N)
end

function centroid(a::AbstractShape{D,T}, b::AbstractShape{D,T}) where {D,T}
    aN, bN = length(vertices(a)), length(vertices(b))
    av, bv = centroid(a), centroid(b)
    N = aN + bN
    x = fill(zero(T), D)
    for i in 1:D
        x[i] += (aN*av[i] + bN*bv[i])/N
    end
    return Vertex(x)
end

function centroid(shapes::AbstractShape{D,T}...) where {D,T}
    x = fill(zero(T), D)
    N = 0
    for v in flatten(map(vertices, shapes))
        N += 1
        for i in 1:D
            x[i] += v[i]
        end
    end
    for i in 1:D
        x[i] /= N
    end
    return Vertex(x)
end


"""
    smallestradius(shape, vertex)

Return the radius of the smallest circle centered at `vertex` that
contains all vertices in `shape`.

"""
smallestradius(s::AbstractShape{D}, p::Vertex{D}) where {D} =
    sqrt(maximum(v -> sum((v[i] - p[i])^2 for i in 1:D), vertices(s)))

function supportvertex(s::AbstractShape{D,T}, a::SVector{D}) where {D,T}
    xmin = Inf
    p = Vertex(zero(T) for _ in 1:D)
    for v in vertices(s)
        x = a ⋅ vector(v)
        x == xmin && (p = centroid(v, p))
        x < xmin && (xmin = x; p = v)
    end
    return p
end

##################################################
# Transformations
##################################################

struct Transformation{D2,T}
    data::NTuple{D2,T}
end
Transformation(data...) = Transformation(promote(data...))

Base.getindex(a::Transformation, idx) = getindex(Core.getfield(a, :data), idx)

(a::Transformation{4})(b::Transformation{4}) = Transformation(
    a[1]*b[1] - a[2]*b[2],
    a[1]*b[2] + a[2]*b[1],
    a[3] + a[1]*b[3] - a[2]*b[4],
    a[4] + a[2]*b[3] + a[1]*b[4])
(t::Transformation{4})(p::Vertex{2}) = Vertex(t[3] + t[1]*p.x- t[2]*p.y, t[4] + t[1]*p.y + t[2]*p.x)
(t::Transformation{4})(v::SVector{2}) = SVector(t[3] + t[1]*v[1]- t[2]*v[2], t[4] + t[1]*v[2] + t[2]*v[1])
(t::Transformation)(l::Polyline) = Polyline(t(p) for p in l)
(t::Transformation)(l::Polygon) = Polygon(t(p) for p in l)
(t::Transformation)(a::HalfSpace{2}) = HalfSpace(
    t(a.vertex),
    SVector(t[1]*a.normal[1]- t[2]*a.normal[2], t[1]*a.normal[2] + t[2]*a.normal[1]))

"""
    translate(x, y)
    translate(shape, x, y)
"""
translate(x, y) = translate(promote(x, y)...)
translate(x::T, y::T) where T = Transformation(one(T), zero(T), x, y)

"""
    rotate(θ)
    rotate(shape, θ)
"""
rotate(θ) = Transformation(cosd(θ), sind(θ), 0., 0.)

"""
    trace(shape, line)

Return an unsorted vector of parameters, corresponding to each
intersection of `line` with `shape`.

"""
function trace(p::Vertex{2}, l::LineSegment{2})
    b = vector(l)
    c = vector(l[1], p)
    # If `d` is zero, the point is in the line.
    if iszero(b × c)
        # We project `p` onto `l`,
        a = (c ⋅ b) / (b ⋅ b)
        # and check if it is outside the bounds:
        amin, amax = parameterrange(l)
        (a <= amin || amax <= a || isnan(a)) && return ()
        # otherwise, we have a match!
        return (a,)
    end
    return ()
end

function trace(l1::LineSegment{2}, l2::LineSegment{2})
    b = vector(l1)
    c = vector(l2)
    k = vector(l1[1], l2[1])
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
            v1, v2 = vertices(l1)
            n = (c ⋅ c)
            a1 = vector(l2[1], v1) ⋅ c / n
            a2 = vector(l2[1], v2) ⋅ c / n
            # Sort the parameters
            a1, a2 = min(a1, a2), max(a1, a2)
            amin2, amax2 = parameterrange(l2)
            # In the 1D projection, we check if there is no overlap,
            (a2 <= amin2 || amax2 <= a1 || isnan(a1) || isnan(a2)) && return ()
            # in which case we return the parameters corresponding to
            # the interval of  overlap of the line segments:
            return (max(a1, amin2), min(a2, amax2))
        end
        # If the lines do not line up, then they do not intersect.
        return ()
    end
    # For non–parallel lines, we check if the parameter `t1/d` is in
    # `l1`, and return `t2/d` if it is in `l2` (TODO: include simple
    # proof why this works).
    t2 = k × b
    amin1, amax1 = parameterrange(l1)
    amin2, amax2 = parameterrange(l2)
    if amin1 <= t1/d <= amax1 && amin2 <= t2/d <= amax2
        return (t2/d,)
    end
    return ()
end

trace(s::AbstractLineShape, l::LineSegment) = flatten(map(e -> trace(e, l), edges(s)))

"""
    intersects(shape1, shape2)
"""
intersects(p1::Vertex, p2::Vertex) = p1 == p2

# See `trace` above for detailed comments of the code.
function intersects(p::Vertex{2}, l::LineSegment{2})
    b = vector(l)
    c = vector(l[1], p)
    if iszero(b × c)
        a = (c ⋅ b) / (b ⋅ b)
        amin, amax = parameterrange(l)
        (a <= amin || amax <= a || isnan(a)) && return false
        return true
    end
    return false
end

# See `trace` above for detailed comments of the code.
function intersects(l1::LineSegment{2}, l2::LineSegment{2})
    b = vector(l1)
    c = vector(l2)
    k = vector(l1[1], l2[1])
    d = b × c
    t1 = k × c
    if iszero(d)
        if iszero(t1)
            v1, v2 = vertices(l1)
            n = (c ⋅ c)
            a1 = vector(l2[1], v1) ⋅ c / n
            a2 = vector(l2[1], v2) ⋅ c / n
            a1, a2 = min(a1, a2), max(a1, a2)
            amin2, amax2 = parameterrange(l2)
            (a2 <= amin2 || amax2 <= a1 || isnan(a1) || isnan(a2)) && return false
            return true
        end
        return false
    end
    t2 = k × b
    amin1, amax1 = parameterrange(l1)
    amin2, amax2 = parameterrange(l2)
    if amin1 <= t1/d <= amax1 && amin2 <= t2/d <= amax2
        return true
    end
    return false
end

intersects(a::AbstractLineShape{N,D}, b::Vertex{D}) where {N,D} = any(e->intersects(e, b), edges(a))
intersects(a::AbstractLineShape{N,D}, b::LineSegment{D}) where {N,D} = any(e->intersects(e, b), edges(a))
intersects(a::Union{Vertex,LineSegment}, b::AbstractLineShape) = intersects(b, a)

const product = Iterators.product
function intersects(a::AbstractLineShape{N1,D}, b::AbstractLineShape{N2,D}) where {N1,N2,D}
    !intersects(BoundingBox(a), BoundingBox(b)) && return false
    for ae in edges(a), be in edges(b)
        intersects(ae, be) && return true
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
function inside(l::AbstractLineShape{N,2}, p::Vertex{2}) where N
    # Ray from p through shape center
    c = centroid(l)
    p2c = normalize(c .- p)
    ray_endpoint = Vertex((p .+ 1e6 * p2c)...)
    return inside(l, p, ray_endpoint)
end
function inside(l::AbstractLineShape{N,2}, p::Vertex{2}, θ::Number) where N
    ray_endpoint = p |> translate(cosd(θ)*1e6, sind(θ)*1e6)
    return inside(l, p, ray_endpoint)
end
function inside(l::AbstractLineShape{N,2}, p1::Vertex{2}, p2::Vertex{2}) where N
    ray = Polyline(p1, p2)
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
            d = vector(ray)
            (i1 && d × vector(ray[1], e[2]) >= 0) && return true
            (i2 && d × vector(ray[1], e[1]) >= 0) && return true
        end
        return false
    end
    return isodd(n)
end

"""
    separatingaxis(a, b)

Return a tuple of `(d, axis)`, where `axis` satisfies the condition
that the projections of `a` and `b` do not overlap with distance `-d`
between them.  If no such axis is found, return instead the `axis`
such that `d*axis` is the minimum translation vector applied to `b`
required to push `b` out of `a`.

"""
function separatingaxis(a::AbstractShape{D,T}, b::AbstractShape{D,T}) where {D,T}
    d = Inf
    axis = zero(SVector{D,T})
    for n in normals(a)
        an = projection(a, n)
        bn = projection(b, n)
        d′ = an[2].x - bn[1].x
        if d′ < d
            d = d′
            axis = n
        end
        d′ = bn[2].x - an[1].x
        if d′ < d
            d = d′
            axis = -n
        end
        d < 0 && return (d, axis)
    end
    return d, axis
end

end #module
