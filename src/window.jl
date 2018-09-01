"""

A Window.

"""
mutable struct Window{T <: AbstractVector{<:AbstractScene}}
    window_ptr::Ptr{SDL.Window}
    render_ptr::Ptr{SDL.Renderer}
    scene_stack::T
    resources::Dict{String, AbstractResource}
    tasks::Vector{Task}

    function Window{T}(title::String, width::Int, height::Int, scene_stack::T = AbstractScene[]; fullscreen::Bool = false) where {T <: AbstractVector{<:AbstractScene}}
        window_ptr = SDL.CreateWindow(title, Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(width), Int32(height), fullscreen ? SDL.WINDOW_FULLSCREEN : UInt32(0))
        render_ptr = SDL.CreateRenderer(window_ptr, Int32(-1), UInt32(0))
        if render_ptr == C_NULL
            SDL.DestroyWindow(window_ptr)
            SDL.DestroyRenderer(render_ptr)
            error(unsafe_string(SDL.GetError()))
        end
        SDL.SetRenderDrawBlendMode(render_ptr, SDL.BLENDMODE_BLEND)
        scene_stack = scene_stack
        resources = Dict{String,AbstractResource}()
        tasks = Task[]
        self = new(window_ptr, render_ptr, scene_stack, resources, tasks)
        finalizer(destroy!, self)
        return self
    end
end

Window(title::String, width::Int, height::Int, scene_stack::T = AbstractScene[]; fullscreen::Bool = false) where {T <: AbstractVector{<:AbstractScene}} = Window{T}(title, width, height, scene_stack, fullscreen=fullscreen)
Window(title::String, width::Int, height::Int, scene::T, scenes::T...; fullscreen::Bool = false) where {T <: AbstractScene} = Window(title, width, height, [scene, scenes...], fullscreen=fullscreen)

Base.show(io::IO, window::Window) = print(io, "Window(\"", unsafe_title(window), "\", ", join(size(window), ", "), ")")

function destroy!(window::Window)
    empty!(window.scene_stack)
    SDL.DestroyWindow(window.window_ptr)
    SDL.DestroyRenderer(window.render_ptr)
    return nothing
end

function Base.getproperty(window::Window, name::Symbol)
    name == :width && return size(window)[1]
    name == :height && return size(window)[2]
    getfield(window, name)
end

"""
    delete!(window::Window, filename::String)

Remove the reference to the resource located at `filename` from `window`.

"""
function delete!(window::Window, filename::String)
    delete!(window.resources, filename)
    return window
end

"""

`Scene` objects run inside a `Window`.  Only one scene can be active
inside a given `Window` at any time.

"""
mutable struct Scene{T <: AbstractVector{<:AbstractLayer}} <: AbstractScene
    layers::T
    color::Colors.RGB
end

Scene(layer::AbstractLayer, layers::AbstractLayer...; color=Colors.colorant"dark gray") = Scene([layer, layers...], color)
Scene(layers::T) where {T<:AbstractVector{<:AbstractLayer}} = Scene(layers, Colors.colorant"dark gray")

struct RenderTask{T <: AbstractResource}
    source::T
    x::Int
    y::Int
    scale_x::Float64
    scale_y::Float64
    angle::Float64
    offset_x::Int
    offset_y::Int
    flip::Symbol
end

"""

A Layer object runs inside a Scene.

"""
mutable struct Layer{T <: AbstractVector{<:AbstractObject}} <: AbstractLayer
    objects::T
    origin_x::Real
    origin_y::Real
    axes::Matrix{<:Real}
    scale::Float64
    show::Bool
    _render_tasks::Vector{RenderTask}

    function Layer{T}(objects::T, origin_x::Real = 0.0, origin_y::Real = 0.0, axes::Matrix{<:Real} = [1. 0.; 0. 1.]; show::Bool = true, scale::Float64 = 1.0) where {T<:AbstractVector{<:AbstractObject}}
        return new(objects, origin_x, origin_y, axes, scale, show, RenderTask[])
    end
end
Layer(objects::T, args...; kwargs...) where {T<:AbstractVector{<:AbstractObject}} = Layer{T}(objects, args...; kwargs...)

Base.show(io::IO, layer::Layer) = print(io, "Layer(", join([layer.objects, layer.show, layer.origin_x, layer.origin_y, layer.axes, layer.scale], ", "), ")")

Base.iterate(layer::Layer) = iterate(layer.objects)
Base.iterate(layer::Layer, state) = iterate(layer.objects, state)

"""
    wait(window::Window)

Block the current task until `window` has finished executing.

"""
function Base.wait(window::Window)
    for task in window.tasks
        wait(task)
    end
    return nothing
end

"""
    size(window::Window)

Return a tuple containing the dimensions of `window`.

"""
function Base.size(window::Window)
    width, height = Int32[0], Int32[0]
    SDL.GetWindowSize(window.window_ptr, pointer(width), pointer(height))
    return width[], height[]
end

unsafe_title(window::Window) = unsafe_string(SDL.GetWindowTitle(window.window_ptr))

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
