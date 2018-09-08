mutable struct Font{T} <: AbstractResource
    ptr::Ptr{T}
    render_ptr::Ptr{SDL.Renderer}
    filename::String
    fontsize::Int
    cache::Dict{UInt64,Texture}

    function Font{T}(ptr, render_ptr, filename, fontsize) where {T}
        self = new{T}(ptr, render_ptr, filename, fontsize, Dict{UInt64,Texture}())
        finalizer(destroy!, self)
        return self
    end
end
Font(ptr::Ptr{T}, render_ptr, filename, fontsize) where {T} = Font{T}(ptr, render_ptr, filename, fontsize)

function Font(resources::Resources, filename::String; fontsize=12)
    if filename in keys(resources)
        @debug("resource already loaded: '$filename'")
        return resources[filename]::Font
    end

    ptr = SDL.TTF_OpenFont(filename, fontsize)
    self = Font(ptr, resources.render_ptr, filename, fontsize)
    finalizer(destroy!, self)
    resources[filename] = self
    return self
end

function destroy!(font::Font{SDL.TTF_Font})
    SDL.TTF_CloseFont(font.ptr)
    font.ptr = C_NULL
    empty!(font.cache)
    return nothing
end

function text(font::Font, txt::String; color=colorant"#303030", halign=0.0, valign=0.0)
    _hash = hash((txt, color, halign, valign))
    if _hash in keys(font.cache)
        return font.cache[_hash]::Texture
    end

    texture = Texture(font, txt, color=color, halign=halign, valign=valign)
    font.cache[_hash] = texture
    return texture
end

function Texture(font::Font, text::String; color=colorant"#303030", halign=0.0, valign=0.0)
    sdl_surface = SDL.TTF_RenderText_Blended(font.ptr, text, SDL.Color(rgba_fromcolor(color)...))
    return Texture(font.render_ptr, sdl_surface, halign=halign, valign=valign)
end
