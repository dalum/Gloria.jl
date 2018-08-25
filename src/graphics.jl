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
        @debug "resource already loaded: \"$fname\""
        return window.resources[fname]::Texture
    end

    for ext in [:svg]
        if match(Regex(".*\\.$ext"), fname) !== nothing
            return Texture(Val(ext), window, fname, args...)
        end
    end
    error("file extension did not match a supported file type")
end

function Texture(::Val{:svg}, window::Window, fname::String)
    rsvg_handle = Rsvg.handle_new_from_file(fname)
    Int(rsvg_handle.ptr) == 0 && error("'$(fname)' is not a valid SVG file")

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

    self = Texture(texture_ptr, width, height, Int(round(width/2)), Int(round(height/2)))
    finalizer(destroy!, self)
    window.resources[fname] = self
    return self
end

function destroy!(texture::Texture)
    SDL.DestroyTexture(texture.ptr)
    return nothing
end

"""
transformpoint(window::Window, scene::AbstractScene, x, y)

Return the coordinates `x` and `y` transformed from the coordinates of
`scene` in the context of `window` to the coordinates of the renderer.

"""
transformpoint(window::Window, scene::AbstractScene, x, y; scale = 1.0) = (window.width // 2 + (x - scene.center_x)*scale, window.height // 2 - (y + scene.center_y)*scale)

"""
transformview(window::Window, scene::AbstractScene, x, y)

Return the coordinates `x` and `y` relative to the view of `scene` in
the context of `window` to the coordinates of the renderer.

"""
transformview(window::Window, scene::AbstractScene, x, y) = (x - window.width // 2, window.height // 2 - y)

"""
transformrelative(window::Window, scene::AbstractScene, rel_x, rel_y)

Return the relative coordinates `rel_x` and `rel_y` transformed from
the coordinates of `scene` in the context of `window` to the
coordinates of the renderer.

"""
transformrelative(window::Window, scene::AbstractScene, rel_x, rel_y) = (rel_x, -rel_y)

"""
transformangle(window::Window, scene::AbstractScene, θ)

Return the angle `θ` transformed from the coordinates of `scene` in
the context of `window` to an angle in the coordinates of the
renderer.

"""
transformangle(window::Window, scene::AbstractScene, θ) = -θ

"""

Set the color for drawing operations on `window`.

"""
setcolor!(window::Window, r::Int, g::Int, b::Int, a::Int) = SDL.SetRenderDrawColor(window.render_ptr, r, g, b, a)
setcolor!(window::Window, color::Colors.Color) = setcolor!(window, Int(round(color.r * 255)), Int(round(color.g * 255)), Int(round(color.b * 255)), Int(round(color.alpha * 255)))
setcolor!(window::Window, color::Colors.Color3) = setcolor!(window, color, 255)
setcolor!(window::Window, color::Colors.Color3, a::Int) = setcolor!(window, Int(round(color.r * 255)), Int(round(color.g * 255)), Int(round(color.b * 255)), a)

"""

Fill `window` with the currently selected color.

"""
clear!(window::Window) = SDL.RenderClear(window.render_ptr)

"""

render!(window::Window, texture::Texture, x, y, args...)

Render `texture` onto `window`'s surface with the texture centered at `x` and `y`.

"""
render!(window::Window, texture::Texture, x, y, args...) = render!(window, texture, Int(round(x)), Int(round(y)), args...)

function render!(window::Window, texture::Texture, x::Int, y::Int)
    x, y = Int.(round.(transformpoint(window, window.scene_stack[end], x, y)))
    center_x, center_y = texture.center_x, texture.center_y
    rect = SDL.Rect(x - center_x, y - center_y, texture.width, texture.height)
    GC.@preserve rect SDL.RenderCopy(window.render_ptr, texture.ptr, C_NULL, pointer_from_objref(rect))
    return window
end

function render!(window::Window, texture::Texture, x::Int, y::Int, θ::Float64, offset_x::Int = 0, offset_y::Int = 0, flip::Symbol = :none)
    x, y = Int.(round.(transformpoint(window, window.scene_stack[end], x, y)))
    θ = transformangle(window, window.scene_stack[end], θ)
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

function render!(window::Window, render_task::RenderTask{Texture})
    texture = render_task.source
    scale = render_task.scale
    x, y = Int.(round.(transformpoint(window, window.scene_stack[end], render_task.x, render_task.y, scale=scale)))
    angle = transformangle(window, window.scene_stack[end], render_task.angle)
    center_x, center_y = Int(round(render_task.source.center_x*scale)), Int(round(render_task.source.center_y*scale))
    flip = UInt32(render_task.flip === :horizontal ? SDL.FLIP_HORIZONTAL :
                  render_task.flip === :vertical ? SDL.FLIP_VERTICAL :
                  SDL.FLIP_NONE)
    rect = SDL.Rect(x - center_x, y - center_y,
                    Int(round(texture.width*scale)),
                    Int(round(texture.height*scale)))
    point = SDL.Point(center_x, center_y)
    GC.@preserve rect point SDL.RenderCopyEx(window.render_ptr, texture.ptr,
                                             C_NULL, pointer_from_objref(rect),
                                             angle, pointer_from_objref(point), flip)
    return window
end

render!(layer::Layer, texture::Texture, x, y; args...) = render!(layer, texture, Int(round(x)), Int(round(y)); args...)

function render!(layer::Layer, texture::Texture, x::Int, y::Int; scale::Float64 = 1.0, angle::Float64 = 0.0, flip::Symbol = :none)
    x = convert(Int, round(cos(layer.angle) * x - sin(layer.angle) * y + layer.origin_x))::Int
    y = convert(Int, round(sin(layer.angle) * x + cos(layer.angle) * y + layer.origin_y))::Int
    scale = convert(Float64, scale*layer.scale)::Float64
    angle = convert(Float64, angle + layer.angle)::Float64
    push!(layer._render_tasks, RenderTask(texture, x, y, scale, angle, flip))
    return layer
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
