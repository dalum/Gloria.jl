mutable struct Font{T} <: AbstractResource
    ptr::Ptr{T}
    filename::String
    fontsize::Int
end

function Font(filename::String; fontsize=12)
    ptr = SDL.TTF_OpenFont(filename, fontsize)
    self = Font(ptr, filename, fontsize)
    finalizer(destroy!, self)
    return self
end

function Font(resources::Resources, filename::String; fontsize=12)
    if filename in keys(resources)
        @debug("resource already loaded: '$filename'")
        return resources[filename]::Font
    end

    self = Font(filename, fontsize=fontsize)
    resources[filename] = self
    return self
end

function destroy!(font::Font{SDL.TTF_Font})
    SDL.TTF_CloseFont(font.ptr)
    font.ptr = C_NULL
    return nothing
end

struct Text{T<:AbstractGraphics} <: AbstractResource
    graphics::T
    text::String
end

function Text(resources::Resources, font::Font, text::String; color=colorant"#303030", halign=0.0, valign=0.0)
    sdl_surface = SDL.TTF_RenderText_Blended(font.ptr, text, SDL.Color(rgba_fromcolor(color)...))
    graphics = Texture(resources, sdl_surface, halign=halign, valign=valign)
    return Text(graphics, text)
end

struct FontRenderer{T}
    font::Font{T}
    resources::Resources
end

function text(fr::FontRenderer, txt::String; color=colorant"#303030", halign=0.0, valign=0.0)
    _hash = hash((fr.font, txt, color, halign, valign))
    if "text_$(_hash)" in keys(fr.resources)
        return fr.resources["text_$(_hash)"]::Text
    end

    t = Text(fr.resources, fr.font, txt, color=color, halign=halign, valign=valign)
    fr.resources["text_$(_hash)"] = t
    return t
end
