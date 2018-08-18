module Graphics

using Gloria: SDL, _render_ptr

setcolor(r::Int, g::Int, b::Int, a::Int) = SDL.SetRenderDrawColor(_render_ptr[], r, g, b, a)
clear() = SDL.RenderClear(_render_ptr[])
fillrect(x::Int, y::Int, w::Int, h::Int) = SDL.RenderFillRect(_render_ptr[], pointer_from_objref(SDL.Rect(x, y, w, h)))
present() = SDL.RenderPresent(_render_ptr[])

end
