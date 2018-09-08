using Cairo
using Rsvg

abstract type AbstractGraphics <: AbstractResource end

mutable struct Texture{T} <: AbstractGraphics
    ptr::Ptr{T}
    width::Int
    height::Int
    center_x::Int
    center_y::Int
end

function Texture(render_ptr::Ptr{SDL.Renderer}, sdl_surface::Ptr{SDL.Surface}; halign = 0.5, valign = 0.5)
    texture_ptr = SDL.CreateTextureFromSurface(render_ptr, sdl_surface)
    SDL.FreeSurface(sdl_surface)
    width, height = Int[0], Int[0]
    SDL.QueryTexture(texture_ptr, C_NULL, C_NULL, width, height)
    self = Texture(texture_ptr, width[], height[], round(Int, width[]*halign), round(Int, height[]*valign))
    finalizer(destroy!, self)
    return self
end

function Texture(resources::Resources, filename::String; width::Int = -1, height::Int = -1, scale::Real = 1.0)
    if filename in keys(resources)
        @debug("resource already loaded: '$filename'")
        return resources[filename]::Texture
    end

    ptr, width, height = load(query(filename), resources, width, height, scale)

    self = Texture(ptr, width, height, round(Int, width/2), round(Int, height/2))
    finalizer(destroy!, self)
    resources[filename] = self
    return self
end

function load(f::File{format"SVG"}, resources::Resources, width::Int, height::Int, scale::Real)
    rsvg_handle = Rsvg.handle_new_from_file(f.filename)
    Int(rsvg_handle.ptr) == 0 && error("'$(f.filename)' is not a valid SVG file")

    rsvg_dim = Rsvg.handle_get_dimensions(rsvg_handle)
    width = ceil(Int, scale*(width >= 0 ? width : rsvg_dim.width))
    height = ceil(Int, scale*(height >= 0 ? height : rsvg_dim.height))

    cairo_surface = Cairo.CairoImageSurface(fill(UInt32(0), (height, width)), Cairo.FORMAT_ARGB32)
    cairo_context = Cairo.CairoContext(cairo_surface)
    scale_x, scale_y = width / rsvg_dim.width, height / rsvg_dim.height
    Cairo.scale(cairo_context, scale_x, scale_y)
    Rsvg.handle_render_cairo(cairo_context, rsvg_handle)
    Cairo.destroy(cairo_context)

    width, height = ceil(Int, cairo_surface.width), ceil(Int, cairo_surface.height)
    sdl_surface = SDL.CreateRGBSurfaceFrom(pointer(cairo_surface.data),
                                           width, height, 32, Int32(width*4),
                                           0x00_ff_00_00, 0x00_00_ff_00, 0x00_00_00_ff, 0xff_00_00_00)
    texture_ptr = SDL.CreateTextureFromSurface(resources.render_ptr, sdl_surface)
    SDL.FreeSurface(sdl_surface)
    Cairo.destroy(cairo_surface)

    if texture_ptr == C_NULL
        error("Failed to load texture from: '$(f.filename)'")
    end

    return texture_ptr, width, height
end

function destroy!(texture::Texture{SDL.Texture})
    SDL.DestroyTexture(texture.ptr)
    texture.ptr = C_NULL
    return nothing
end
