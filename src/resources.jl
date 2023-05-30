abstract type AbstractResource end

struct Resources{T<:AbstractResource}
    render_ptr::Ptr{SDL.SDL_Renderer}
    d::Dict{String, T}
end
Resources{T}(window::Window) where {T} = Resources(window.render_ptr, Dict{String, T}())
Resources(window::Window) = Resources{AbstractResource}(window)

Base.getindex(r::Resources, key) = getindex(r.d, key)
Base.setindex!(r::Resources, key, val) = setindex!(r.d, key, val)
Base.keys(r::Resources) = keys(r.d)
Base.values(r::Resources) = values(r.d)
Base.delete!(r::Resources{T}, key::T) where {T<:AbstractResource} = (delete!(r.d, key); r)

