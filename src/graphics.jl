module Graphics

import Gloria: render!

using Gloria: AbstractResource, SDL, Window
using Cairo
using Rsvg

mutable struct Texture <: AbstractResource
    ptr::Ptr{SDL.Texture}
    width::Int
    height::Int
    center_x::Int
    center_y::Int
end

function Texture(window::Window, fname::String, args...)
    if fname in keys(window.resources)
        return window.resources[fname]
    end

    for ext in [:svg]
        if match(Regex(".*\\.$ext"), fname) !== nothing
            return Texture(Val{ext}, window, fname, args...)
        end
    end
    error("file extension did not match a supported file type")
end

function Texture(::Type{Val{:svg}}, window::Window, fname::String)
    rsvg_handle = Rsvg.handle_new_from_file(fname)
    Int(rsvg_handle.ptr) == 0 && error("'$(fname)' is not a valid SVG file")

    rsvg_dim = Rsvg.handle_get_dimensions(rsvg_handle)
    # hypotenuse = Int(ceil(hypot(rsvg_dim.width, rsvg_dim.height)))
    cairo_surface = Cairo.CairoImageSurface(fill(UInt32(0), (rsvg_dim.height, rsvg_dim.width)), Cairo.FORMAT_ARGB32)
    cairo_context = Cairo.CairoContext(cairo_surface)
    # Cairo.translate(cairo_context, (hypotenuse - rsvg_dim.width)/2, (hypotenuse - rsvg_dim.height)/2)
    # Cairo.translate(cairo_context, 100.0, 100.0)
    Rsvg.handle_render_cairo(cairo_context, rsvg_handle)
    Cairo.destroy(cairo_context)

    width, height = Int(ceil(cairo_surface.width)), Int(ceil(cairo_surface.height))
    sdl_surface = SDL.CreateRGBSurfaceFrom(pointer(cairo_surface.data),
                                           width, height, 32, Int32(width*4),
                                           0x00_ff_00_00, 0x00_00_ff_00, 0x00_00_00_ff, 0xff_00_00_00)
    texture_ptr = SDL.CreateTextureFromSurface(window.render_ptr, sdl_surface)
    SDL.FreeSurface(sdl_surface)
    Cairo.destroy(cairo_surface)

    self = Texture(texture_ptr, width, height, Int(round(width/2)), Int(round(height/2)))
    finalizer(destroy!, self)
    window.resources[fname] = self
    return self
end

function destroy!(texture::Texture)
    SDL.DestroyTexture(texture.ptr)
    return nothing
end

# struct VectorGraphics
#     width::Int
#     height::Int
#     hypotenuse::Int
#     handle::Rsvg.RsvgHandle
#     surface::Cairo.CairoSurface
# end

# """

# Create a VectorGraphics object from an SVG file and pad it with
# whitespace to allow rotations.

# """
# function VectorGraphics(fname::String)
#     handle = Rsvg.handle_new_from_file(fname)
#     Int(handle.ptr) == 0 && error("'$(fname)' is not a valid SVG file")
#     dim = Rsvg.handle_get_dimensions(handle)
#     hypotenuse = Int(ceil(hypot(dim.width, dim.height)))
#     surface = Cairo.CairoImageSurface(fill(UInt32(0), (hypotenuse, hypotenuse)), Cairo.FORMAT_ARGB32)
#     return VectorGraphics(dim.width, dim.height, hypotenuse, handle, surface)
# end

# function render!(svg::VectorGraphics; rotate::Real = 0.0)
#     svg.surface.data .= 0x00_00_00_00
#     context = Cairo.CairoContext(svg.surface)
#     Cairo.translate(context, (svg.hypotenuse - svg.width)/2, (svg.hypotenuse - svg.height)/2)
#     Cairo.translate(context, svg.width/2, svg.height/2)
#     Cairo.rotate(context, rotate)
#     Cairo.translate(context, -svg.width/2, -svg.height/2)
#     Rsvg.handle_render_cairo(context, svg.handle)
#     # Cairo.destroy(surface)
#     Cairo.destroy(context)
# end

setcolor!(window::Window, r::Int, g::Int, b::Int, a::Int) = SDL.SetRenderDrawColor(window.render_ptr, r, g, b, a)

clear!(window::Window) = SDL.RenderClear(window.render_ptr)

"""

render!(window::Window, texture::Texture, x, y, args...)

Render `texture` onto `window`'s surface with the texture centered at `x` and `y`.

"""
render!(window::Window, texture::Texture, x, y, args...) = render!(window, texture, Int(round(x)), Int(round(y)), args...)

function render!(window::Window, texture::Texture, x::Int, y::Int)
    center_x, center_y = texture.center_x, texture.center_y
    rect = SDL.Rect(x - center_x, y - center_y, texture.width, texture.height)
    GC.@preserve rect SDL.RenderCopy(window.render_ptr, texture.ptr, C_NULL, pointer_from_objref(rect))
    return window
end

function render!(window::Window, texture::Texture, x::Int, y::Int, θ::Float64, offset_x::Int = 0, offset_y::Int = 0, flip::Symbol = :none)
    center_x, center_y = texture.center_x + offset_x, texture.center_y + offset_y
    rect = SDL.Rect(x - center_x, y - center_y, texture.width, texture.height)
    point = SDL.Point(center_x, center_y)
    flip = UInt32(flip === :horizontal ? SDL.FLIP_HORIZONTAL :
                  flip === :vertical ? SDL.FLIP_VERTICAL :
                  SDL.FLIP_NONE)
    GC.@preserve rect point SDL.RenderCopyEx(window.render_ptr, texture.ptr,
                                             C_NULL, pointer_from_objref(rect),
                                             θ, pointer_from_objref(point),
                                             flip)
    return window
end

"""
"""
function drawrect!(window::Window, x::Int, y::Int, w::Int, h::Int)
    rect = SDL.Rect(x, y, w, h)
    GC.@preserve rect SDL.RenderDrawRect(window.render_ptr, pointer_from_objref(rect))
    return window
end

"""
"""
function fillrect!(window::Window, x::Int, y::Int, w::Int, h::Int)
    rect = SDL.Rect(x, y, w, h)
    GC.@preserve rect SDL.RenderFillRect(window.render_ptr, pointer_from_objref(rect))
    return window
end

"""
"""
drawline!(window::Window, (x1, y1)::Tuple{Int,Int}, (x2, y2)::Tuple{Int,Int}) = SDL.RenderDrawLine(window.render_ptr, Int32(x1), Int32(y1), Int32(x2), Int32(y2))

"""
"""
drawpoint!(window::Window, (x, y)::Tuple{Int,Int}) = SDL.RenderDrawPoint(window.render_ptr, Int32(x), Int32(y))

"""
"""
present!(window::Window) = SDL.RenderPresent(window.render_ptr)

end
