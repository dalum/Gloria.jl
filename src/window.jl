"""

A Window is the main GUI element.

"""
mutable struct Window{T <: AbstractScene}
    window_ptr::Ptr{SDL.Window}
    render_ptr::Ptr{SDL.Renderer}
    scene_stack::Vector{T}
    resources::Dict{String, AbstractResource}
    tasks::Vector{Task}
end

"""

Create a Window with a title, size, and base scene.

"""
Window(args...; kwargs...) = Window{AbstractScene}(args...; kwargs...)
function Window{T}(title::String, width::Int, height::Int; fullscreen::Bool = false, show_cursor::Bool = true) where {T <: AbstractScene}
    window_ptr = SDL.CreateWindow(title, Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(width), Int32(height), fullscreen ? SDL.WINDOW_FULLSCREEN : UInt32(0))
    render_ptr = SDL.CreateRenderer(window_ptr, Int32(-1), UInt32(SDL.RENDERER_ACCELERATED | SDL.RENDERER_PRESENTVSYNC | SDL.RENDERER_TARGETTEXTURE))
    SDL.SetRenderDrawBlendMode(render_ptr, SDL.BLENDMODE_BLEND)
    scene_stack = T[]
    resources = Dict{String,AbstractResource}()
    tasks = Task[]
    SDL.ShowCursor(Int32(show_cursor))
    window = Window(window_ptr, render_ptr, scene_stack, resources, tasks)
    finalizer(destroy!, window)
    push!(_windows, window)
    return window
end

function destroy!(window::Window)
    empty!(window.scene_stack)
    SDL.DestroyWindow(window.window_ptr)
    SDL.DestroyRenderer(window.render_ptr)
    deleteat!(_windows, findall(x->x===window, _windows))
    return nothing
end

function Base.getproperty(window::Window, name::Symbol)
    name == :width && return size(window)[1]
    name == :height && return size(window)[2]
    getfield(window, name)
end

"""

A Scene object runs inside a Window.  Only one scene can be active
inside the window at any time.

"""
mutable struct Scene{T <: AbstractLayer} <: AbstractScene
    layers::Vector{T}
    center_x::Int
    center_y::Int
    color::Colors.RGB

    function Scene{T}(layers::Vector{T}, center_x::Int = 0, center_y::Int = 0, color::Colors.RGB = Colors.colorant"dark gray") where {T <: AbstractLayer}
        return new{T}(layers, center_x, center_y, color)
    end
end

Scene(layers::Vector{T}, args...) where {T <: AbstractLayer} = Scene{T}(layers, args...)

Scene() = Scene{AbstractLayer}(AbstractLayer[])
Scene{T}() where {T <: AbstractLayer} = Scene(T[])

struct RenderTask{T <: AbstractResource}
    source::T
    x::Int
    y::Int
    scale::Float64
    angle::Float64
    flip::Symbol
end

"""

A Layer object runs inside a Scene.

"""
mutable struct Layer{T <: AbstractObject} <: AbstractLayer
    sortby
    objects::Vector{AbstractObject}
    show::Bool
    scale::Float64
    origin_x::Int
    origin_y::Int
    angle::Float64
    _render_tasks::Vector{RenderTask}

    function Layer{T}(sortby, objects::Vector{T}; show::Bool = true, scale::Float64 = 1.0, offset_x::Int = 0, offset_y::Int = 0, angle::Float64 = 0.0) where {T <: AbstractObject}
        return new(sortby, objects, show, scale, offset_x, offset_y, angle, RenderTask[])
    end
end

Layer(sortby, objects::Vector{T}; args...) where {T <: AbstractObject} = Layer{T}(sortby, objects; args...)

Layer(sortby::Function; args...) = Layer(sortby, AbstractObject[]; args...)
Layer{T}(sortby::Function; args...) where {T <: AbstractObject} = Layer(sortby, T[]; args...)

"""
wait(window::Window)

Block the current task until `window` has finished executing.

"""
Base.wait(window::Window) = wait.(window.tasks)

"""
size(window::Window)

Return a tuple containing the dimensions of `window`.

"""
function Base.size(window::Window)
    width, height = Int32[0], Int32[0]
    SDL.GetWindowSize(window.window_ptr, pointer(width), pointer(height))
    return width[], height[]
end

"""
push!(window::Window, scenes::AbstractScene...)

Add one or more `scenes` to the top of the scene stack of `window`.

"""
Base.push!(window::Window, scenes::AbstractScene...) = push!(window.scene_stack, scenes...)

Base.pop!(window::Window) = pop!(window.scene_stack)

"""
append!(window::Window, scenes::AbstractVector{<:AbstractScene})

Add the elements of `scenes` to the the top of the scene stack of
`window`.

"""
Base.append!(window::Window, scenes::AbstractVector{<:AbstractScene}) = append!(window.scene_stack, scenes)

"""
push!(scene::AbstractScene, layers::AbstractLayer...)

Add one or more `layers` to `scene`.

"""
Base.push!(scene::AbstractScene, layers::AbstractLayer...) = push!(scene.layers, layers...)

Base.pop!(scene::AbstractScene) = pop!(scene.layers)

"""
append!(scene::AbstractScene, objs::AbstractVector{<:AbstractLayer})

Add the layers in `layers` to `scene`.

"""
Base.append!(scene::AbstractScene, layers::AbstractVector{<:AbstractLayer}) = append!(scene.layers, layers)

"""
push!(layer::AbstractLayer, objs::AbstractObject...)

Add one or more `objs` to `layer`.

"""
Base.push!(layer::AbstractLayer, objs::AbstractObject...) = push!(layer.objects, objs...)

Base.pop!(scene::AbstractLayer) = pop!(layer.objects)

"""
append!(layer::AbstractLayer, objs::AbstractVector{<:AbstractObject})

Add the objects in `objs` to `layer`.

"""
Base.append!(layer::AbstractLayer, objs::AbstractVector{<:AbstractObject}) = append!(layer.objects, objs)
