"""

Set the color for drawing operations on `window`.

"""
setcolor!(window::Window, r::Int, g::Int, b::Int, a::Int) = SDL.SetRenderDrawColor(window.render_ptr, r, g, b, a)
setcolor!(window::Window, color::Colors.Color) = setcolor!(window, round(Int, color.r * 255), round(Int, color.g * 255), round(Int, color.b * 255), round(Int, color.alpha * 255))
setcolor!(window::Window, color::Colors.Color3) = setcolor!(window, color, 255)
setcolor!(window::Window, color::Colors.Color3, a::Int) = setcolor!(window, round(Int, color.r * 255), round(Int, color.g * 255), round(Int, color.b * 255), a)

"""
    clear!(window)

Fill `window` with the currently selected color.

"""
clear!(window::Window) = SDL.RenderClear(window.render_ptr)

"""
    present!(window)

Update `window` to display all render operations since the last call
to `present!`.

"""
present!(window::Window) = SDL.RenderPresent(window.render_ptr)

##################################################
# Textures
##################################################

"""
    render!(window::Window, texture::Texture, x, y, args...)

Render `texture` onto `window`'s surface with the texture centered at
`x` and `y`.

"""
function render!(window::Window, texture::Texture{SDL.Texture}, x::Int, y::Int, θ::Float64 = 0.0, scale_x::Float64 = 1.0, scale_y::Float64 = 1.0, offset_x::Int = 0, offset_y::Int = 0, flip::Symbol = :none)
    center_x, center_y = (texture.center_x + offset_x)*scale_x, (texture.center_y + offset_y)*scale_y
    rect = SDL.Rect(round(Int, x - center_x), round(Int, y - center_y),
                    round(Int, texture.width*scale_x), round(Int, texture.height*scale_y))
    point = SDL.Point(round(Int, center_x), round(Int, center_y))
    flip = UInt32(flip === :horizontal ? SDL.FLIP_HORIZONTAL :
                  flip === :vertical ? SDL.FLIP_VERTICAL :
                  SDL.FLIP_NONE)
    GC.@preserve rect point SDL.RenderCopyEx(window.render_ptr, texture.ptr,
                                             C_NULL, pointer_from_objref(rect),
                                             θ, pointer_from_objref(point),
                                             flip)
    return window
end

function render!(window::Window, layer::AbstractLayer, r::RenderTask{<:Texture})
    x = r.x*layer.scale + layer.x
    y = r.y*layer.scale + layer.y
    scale = layer.scale*r.scale
    render!(window, r.source, round(Int, x), round(Int, y), r.θ, scale, scale, r.offset_x, r.offset_y, r.flip)
end

function render!(layer::Layer, texture::Texture, x, y, θ = 0.0; scale = 1.0, offset_x = 0, offset_y = 0, flip::Symbol = :none)
    push!(layer.render_tasks, RenderTask(texture, x, y, θ, scale, offset_x, offset_y, flip))
    return layer
end

##################################################
# Shapes
##################################################

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
function drawpoint!(window::Window, x::Int, y::Int)
    SDL.RenderDrawPoint(window.render_ptr, Int32(x), Int32(y))
    return window
end

"""
"""
function drawline!(window::Window, x1::Int, y1::Int, x2::Int, y2::Int)
    SDL.RenderDrawLine(window.render_ptr, Int32(x1), Int32(y1), Int32(x2), Int32(y2))
    return window
end

##################################################

function render!(window::Window, p::Point, x, y; scale = 1.0)
    drawpoint!(window, round(Int, x + p.x), round(Int, y + p.y))
end

function render!(window::Window, l::Line, x, y; scale = 1.0)
    x1 = x + l.x1*scale
    x2 = x + l.x2*scale
    y1 = y + l.y1*scale
    y2 = y + l.y2*scale
    drawline!(window, round(Int, x1), round(Int, y1), round(Int, x2), round(Int, y2))
end

function render!(window::Window, p::Polygon, x, y; scale = 1.0)
    for line in p.lines
        render!(window, line, x, y, scale=scale)
    end
    return window
end

function render!(window::Window, cs::CompositeShape, x, y; scale = 1.0)
    for shape in cs.shapes
        render!(window, shape, x, y, scale=scale)
    end
    return window
end

function render!(layer::Layer, shape::AbstractShape, x, y, θ = 0.0; scale = 1.0, offset_x = 0, offset_y = 0, flip::Symbol = :none)
    push!(layer.render_tasks, RenderTask(shape, x, y, θ, scale, offset_x, offset_y, flip))
    return layer
end

function render!(window::Window, layer::AbstractLayer, r::RenderTask{<:AbstractShape})
    x = r.x*layer.scale + layer.x
    y = r.y*layer.scale + layer.y
    scale = layer.scale*r.scale
    render!(window, r.source, x, y, scale=scale)
end
