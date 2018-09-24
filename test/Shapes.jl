module TestShapes

using Test, Random

using Gloria.Shapes
using Gloria.Shapes: closestprojection, edges, extrude, inside, intersects, projection, rotate, trace, translate, vertices

@testset "Intersection" begin
    p0 = Point(0., 0.)
    p1 = Point(1.05, 0.)
    p2 = Point(1.5, 0.25)
    p3 = Point(-0.5, 1.25)
    l1 = Line(Point(-1., 0.), Point(1., 0.))
    l2 = Line(Point(0.5, 0.), Point(1.5, 0.))
    l3 = Line(Point(0.85, -0.5), Point(1.85, 0.5))
    l4 = Line(Point(-0.5, 1.), Point(-0.5, 2.))
    l5 = Line(Point(-0.25, -0.75), Point(0.75, 0.25))
    pol1 = polygon([Point(-1., -0.5), Point(0., 0.5), Point(1., -0.5), Point(0., -1.5)])
    pol2 = polygon([Point(-0.5, -0.5), Point(-0.5, 0.5), Point(0.5, 0.5), Point(0.5, -0.5)])
    pol3 = polygon(reverse([Point(-0.5, -0.5), Point(-0.5, 0.5), Point(0.5, 0.5), Point(0.5, -0.5)]))

    c1 = circle(Point(0., 0.), 1., samples=4)
    c2 = circle(Point(0., 3.), 1., samples=4)

    @test p0 == zero(p1) == zero(typeof(p2))
    @test iszero(p0)

    @test p1[1] === p1
    @test p1[1:1] === (p1,)
    @test_throws BoundsError p1[2]

    @test trace(p0, l1) == (0.5,)
    @test trace(p0, l2) == ()
    @test trace(p0, l3) == ()
    @test trace(p0, l4) == ()
    @test trace(p0, l5) == ()

    @test trace(p1, l1) == ()
    @test trace(p1, l2) == (0.55,)
    @test trace(p2, l1) == trace(p2, l2) == ()
    @test trace(p2, l3) == () # Floating point errors means this will never hit

    @test trace(l1, l2) == (0., 0.5)
    @test trace(l2, l1) == (0.75, 1.)
    @test trace(l3, l1) == ()
    @test trace(l3, l2) == (0.85,)
    @test trace(l3, l1) == trace(l1, l3) == ()

    @test collect(Float64, trace(pol1, l1)) == [0.25, 0.75]
    @test collect(Float64, trace(pol1, l4)) == Float64[]
    @test collect(Float64, trace(pol1, (l1, l2))) == [0.25, 0.75, 0.]
    @test collect(Float64, trace(pol1, l3)) ≈ [0.075]
    @test collect(Float64, trace(pol1, (l1, l2, l3, l4))) ≈ [0.25, 0.75, 0., 0.075]

    @test intersects(pol1, l1)
    @test intersects(l1, pol1)
    @test intersects(pol1, l3)
    @test intersects(l3, pol1)
    @test !intersects(pol1, l4)
    @test !intersects(l4, pol1)

    for θ in 0:10:360
        @eval @test count(q -> inside(q, $p0, $θ), edges($pol1)) == 1
        @eval @test inside($pol1, $p0, $θ)
        @eval @test !inside($pol1, $p1, $θ)
        @eval @test !inside($pol1, $p2, $θ)
        @eval @test !inside($pol2, $p3, $θ)
        @eval @test !inside($pol3, $p3, $θ)
    end
    for θ in 0:10:360, v in unique!(collect(vertices(c2)))
        @eval @test !inside($c1, $v, $θ)
    end
end

@testset "Transformations" begin
    p = Point(0., 0.)
    l = extrude(p, translate(1, 0))
    sqr = extrude(l, translate(0, 1))

    @test translate(p, 2, 0) == p |> translate(2, 0) == Point(2., 0.)
    @test rotate(translate(p, 1, 0), 90) == p |> translate(1, 0) |> rotate(90) == Point(0., 1.)
    @test p |> translate(1, 0) |> rotate(90) == p |> rotate(90) |> translate(0, 1.) == Point(0., 1.)

    @test l == Line(Point(0., 0.), Point(1., 0.))
    @test l |> translate(1, 0) |> rotate(90) == l |> rotate(90) |> translate(0, 1.) == Line(Point(0., 1.), Point(0., 2.))

    @test sqr == [Line(Point(0., 0.), Point(1., 0.)), Line(Point(0., 1.), Point(1., 1.)), Line(Point(0., 0.), Point(0., 1.)), Line(Point(1., 0.), Point(1., 1.))]
end

@testset "Projections" begin
    p1 = Point(0., 0.)
    p2 = Point(1., 0.8)
    l = Line(Point(-1.0, -0.5), Point(1.0, -0.5))
    shape = extrude(l, translate(1, 1))
    @test projection(l, p1) == Point(0., -0.5)
    @test projection(l, p2) == Point(1.0, -0.5)
    @test closestprojection(shape, p1) == Point(-0.25, 0.25)
end

end #module
