using Cairo
using Rsvg

mutable struct Texture <: AbstractResource
    ptr::Ptr{SDL.Texture}
    fname::String
    width::Int
    height::Int
    center_x::Int
    center_y::Int
end

function Texture(window::Window, fname::String)
    if fname in keys(window.resources)
        @debug("resource already loaded: '$fname'")
        return window.resources[fname]::Texture
    end

    ptr, width, height = load(query(fname), window)

    self = Texture(ptr, fname, width, height, Int(round(width/2)), Int(round(height/2)))
    finalizer(destroy!, self)
    window.resources[fname] = self
    return self
end

function load(f::File{format"SVG"}, window::Window)
    rsvg_handle = Rsvg.handle_new_from_file(f.filename)
    Int(rsvg_handle.ptr) == 0 && error("'$(f.filename)' is not a valid SVG file")

    rsvg_dim = Rsvg.handle_get_dimensions(rsvg_handle)
    cairo_surface = Cairo.CairoImageSurface(fill(UInt32(0), (rsvg_dim.height, rsvg_dim.width)), Cairo.FORMAT_ARGB32)
    cairo_context = Cairo.CairoContext(cairo_surface)
    Rsvg.handle_render_cairo(cairo_context, rsvg_handle)
    Cairo.destroy(cairo_context)

    width, height = Int(ceil(cairo_surface.width)), Int(ceil(cairo_surface.height))
    sdl_surface = SDL.CreateRGBSurfaceFrom(pointer(cairo_surface.data),
                                           width, height, 32, Int32(width*4),
                                           0x00_ff_00_00, 0x00_00_ff_00, 0x00_00_00_ff, 0xff_00_00_00)
    texture_ptr = SDL.CreateTextureFromSurface(window.render_ptr, sdl_surface)
    SDL.FreeSurface(sdl_surface)
    Cairo.destroy(cairo_surface)
    return texture_ptr, width, height
end

function destroy!(t::Texture)
    SDL.DestroyTexture(t.ptr)
    t.ptr = C_NULL
    return nothing
end

"""

Set the color for drawing operations on `window`.

"""
setcolor!(window::Window, r::Int, g::Int, b::Int, a::Int) = SDL.SetRenderDrawColor(window.render_ptr, r, g, b, a)
setcolor!(window::Window, color::Colors.Color) = setcolor!(window, round(Int, color.r * 255), round(Int, color.g * 255), round(Int, color.b * 255), round(Int, color.alpha * 255))
setcolor!(window::Window, color::Colors.Color3) = setcolor!(window, color, 255)
setcolor!(window::Window, color::Colors.Color3, a::Int) = setcolor!(window, round(Int, color.r * 255), round(Int, color.g * 255), round(Int, color.b * 255), a)

"""

Fill `window` with the currently selected color.

"""
clear!(window::Window) = SDL.RenderClear(window.render_ptr)

"""

render!(window::Window, texture::Texture, x, y, args...)

Render `texture` onto `window`'s surface with the texture centered at `x` and `y`.

"""
function render!(window::Window, texture::Texture, x::Int, y::Int, scale_x::Float64 = 1.0, scale_y::Float64 = 1.0, angle::Float64 = 0.0, offset_x::Int = 0, offset_y::Int = 0, flip::Symbol = :none)
    center_x, center_y = (texture.center_x + offset_x)*scale_x, (texture.center_y + offset_y)*scale_y
    rect = SDL.Rect(round(Int, x - center_x), round(Int, y - center_y),
                    round(Int, texture.width*scale_x), round(Int, texture.height*scale_y))
    point = SDL.Point(round(Int, center_x), round(Int, center_y))
    flip = UInt32(flip === :horizontal ? SDL.FLIP_HORIZONTAL :
                  flip === :vertical ? SDL.FLIP_VERTICAL :
                  SDL.FLIP_NONE)
    GC.@preserve rect point SDL.RenderCopyEx(window.render_ptr, texture.ptr,
                                             C_NULL, pointer_from_objref(rect),
                                             angle, pointer_from_objref(point),
                                             flip)
    return window
end

render!(window::Window, r::RenderTask{Texture}) = render!(window, r.source, r.x, r.y, r.scale_x, r.scale_y, r.angle, r.offset_x, r.offset_y, r.flip)

function render!(layer::Layer, texture::Texture, pos::Vector{<:Real}; scale_x::Float64 = 1.0, scale_y::Float64 = 1.0, angle::Float64 = 0.0, offset_x::Int = 0, offset_y::Int = 0, flip::Symbol = :none)
    x, y = round.(Int, (layer.axes*pos).*layer.scale .+ [layer.origin_x, layer.origin_y])
    scale_x *= layer.scale
    scale_y *= layer.scale
    push!(layer._render_tasks, RenderTask(texture, x, y, scale_x, scale_y, angle, offset_x, offset_y, flip))
    return layer
end

render!(layer::Layer, texture::Texture, xs::T...; args...) where {T<:Real} = render!(layer, texture, collect(xs); args...)

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
