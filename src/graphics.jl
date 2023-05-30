"""

Set the color for drawing operations on `window`.

"""
setcolor!(window::Window, r::Int, g::Int, b::Int, a::Int) = SDL.SDL_SetRenderDrawColor(window.render_ptr, r, g, b, a)
setcolor!(window::Window, color::Colors.Color) = setcolor!(window, rgba_fromcolor(color)...)
setcolor!(window::Window, color::Colors.Color, a::Int) = setcolor!(window, rgba_fromcolor(color, a)...)

"""
    clear!(window)

Fill `window` with the currently selected color.

"""
clear!(window::Window) = SDL.SDL_RenderClear(window.render_ptr)

"""
    present!(window)

Update `window` to display all render operations since the last call
to `present!`.

"""
present!(window::Window) = SDL.SDL_RenderPresent(window.render_ptr)

##################################################
# Generic rendering
##################################################

function render!(layer::AbstractLayer, source::Union{AbstractGraphics,AbstractShape}, x, y, θ = 0.0; scale = 1.0, offset_x = 0, offset_y = 0, flip::Symbol = :none, color = colorant"black")
    push!(layer.render_tasks, RenderTask(source, x, y, θ, scale, offset_x, offset_y, flip, color))
    return layer
end

function render!(layer::AbstractLayer, text::Text, x, y, θ = 0.0; scale = 1.0, offset_x = 0, offset_y = 0, flip::Symbol = :none, color = colorant"black")
    push!(layer.render_tasks, RenderTask(text.graphics, x, y, θ, scale, offset_x, offset_y, flip, color))
    return layer
end

##################################################
# AbstractGraphics
##################################################

function render!(window::Window, layer::AbstractLayer, r::RenderTask{<:AbstractGraphics})
    x = r.x*layer.scale + layer.x
    y = r.y*layer.scale + layer.y
    scale = layer.scale*r.scale
    render!(window, r.source, round(Int, x), round(Int, y), r.θ, scale, scale, r.offset_x, r.offset_y, r.flip)
end

function render!(window::Window, source::Texture{SDL.SDL_Texture}, x::Int, y::Int, θ::Float64 = 0.0, scale_x::Float64 = 1.0, scale_y::Float64 = 1.0, offset_x::Int = 0, offset_y::Int = 0, flip::Symbol = :none)
    center_x, center_y = (source.center_x + offset_x)*scale_x, (source.center_y + offset_y)*scale_y
    rect = SDL.SDL_Rect(round(Int, x - center_x), round(Int, y - center_y),
                    round(Int, source.width*scale_x), round(Int, source.height*scale_y))
    point = SDL.SDL_Point(round(Int, center_x), round(Int, center_y))
    flip = flip === :horizontal ? SDL.SDL_FLIP_HORIZONTAL :
                  flip === :vertical ? SDL.SDL_FLIP_VERTICAL :
                  SDL.SDL_FLIP_NONE
    GC.@preserve rect point SDL.SDL_RenderCopyEx(window.render_ptr, source.ptr,
                                             C_NULL, Ref(rect),
                                             θ, Ref(point),
                                             flip)
    return window
end

##################################################
# Shapes
##################################################

"""
"""
function drawrect!(window::Window, x::Int, y::Int, w::Int, h::Int)
    rect = SDL.SDL_Rect(x, y, w, h)
    GC.@preserve rect SDL.SDL_RenderDrawRect(window.render_ptr, pointer_from_objref(rect))
    return window
end

"""
"""
function fillrect!(window::Window, x::Int, y::Int, w::Int, h::Int)
    rect = SDL.SDL_Rect(x, y, w, h)
    GC.@preserve rect SDL.SDL_RenderFillRect(window.render_ptr, pointer_from_objref(rect))
    return window
end

"""
"""
function drawpoint!(window::Window, x::Int, y::Int)
    SDL.SDL_RenderDrawPoint(window.render_ptr, Int32(x), Int32(y))
    return window
end

"""
"""
function drawline!(window::Window, x1::Int, y1::Int, x2::Int, y2::Int)
    SDL.SDL_RenderDrawLine(window.render_ptr, Int32(x1), Int32(y1), Int32(x2), Int32(y2))
    return window
end

##################################################

function render!(window::Window, p::Point{2}, x, y, θ = 0.0; scale = 1.0, color)
    setcolor!(window, color)
    drawpoint!(window, round(Int, x + first(p).x), round(Int, y + first(p).y))
end

function render!(window::Window, l::AbstractLineShape{N,2}, x, y, θ = 0.0; scale = 1.0, color) where N
    setcolor!(window, color)
    for e in edges(l |> rotate(θ) |> translate(x, y))
        @inbounds x1 = e[1].x*scale
        @inbounds x2 = e[2].x*scale
        @inbounds y1 = e[1].y*scale
        @inbounds y2 = e[2].y*scale
        drawline!(window, round(Int, x1), round(Int, y1), round(Int, x2), round(Int, y2))
    end
end

function render!(window::Window, l::HalfPlane, x, y, θ = 0.0; scale = 1.0, color)
    setcolor!(window, color)
    for e in edges(l |> rotate(θ) |> translate(x, y))
        @inbounds x1 = e[1].x*scale
        @inbounds x2 = e[2].x*scale
        @inbounds y1 = e[1].y*scale
        @inbounds y2 = e[2].y*scale
        drawline!(window, round(Int, x1), round(Int, y1), round(Int, x2), round(Int, y2))
    end
end

function render!(window::Window, layer::AbstractLayer, r::RenderTask{<:AbstractShape})
    x = r.x*layer.scale + layer.x
    y = r.y*layer.scale + layer.y
    scale = layer.scale*r.scale
    render!(window, r.source, x, y, r.θ, scale=scale, color=r.color)
end
