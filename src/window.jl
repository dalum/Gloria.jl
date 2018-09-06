abstract type AbstractScene end
abstract type AbstractLayer end

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
function Base.delete!(window::Window, filename::String)
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

struct RenderTask{T}
    source::T
    x::Float64
    y::Float64
    θ::Float64
    scale::Float64
    offset_x::Int
    offset_y::Int
    flip::Symbol
end

"""

A Layer object runs inside a Scene.

"""
mutable struct Layer{T} <: AbstractLayer
    objects::OrderedSet{T}
    x::Float64
    y::Float64
    θ::Float64
    scale::Float64
    show::Bool
    render_tasks::Vector{RenderTask}
    new_objects::OrderedSet{T}
    dead_objects::Set{T}

    function Layer{T}(objects, x = 0.0, y = 0.0, θ = 0.0; show = true, scale = 1.0) where {T}
        return new(objects, x, y, θ, scale, show, RenderTask[], OrderedSet{T}(), Set{T}())
    end
end
Layer(objects::OrderedSet{T}, args...; kwargs...) where {T} = Layer{T}(objects, args...; kwargs...)
Layer(objects::Vector, args...; kwargs...) = Layer(OrderedSet(objects), args...; kwargs...)

Base.show(io::IO, layer::Layer) = print(io, "Layer(", join([layer.objects, layer.x, layer.y, layer.θ], ", "), ")")

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

Base.push!(window::Window, scenes::AbstractScene...) = push!(window.scene_stack, scenes...)
Base.pop!(window::Window) = pop!(window.scene_stack)
Base.append!(window::Window, scenes::AbstractVector{<:AbstractScene}) = append!(window.scene_stack, scenes)
Base.push!(scene::AbstractScene, layers::AbstractLayer...) = push!(scene.layers, layers...)
Base.pop!(scene::AbstractScene) = pop!(scene.layers)
Base.append!(scene::AbstractScene, layers::AbstractVector{<:AbstractLayer}) = append!(scene.layers, layers)
Base.push!(layer::Layer, objs...) = (push!(layer.objects, objs...); layer)
Base.delete!(layer::Layer, obj) = (delete!(layer.objects, obj); layer)

"""

"""
add!(layer::Layer, objs...) = (push!(layer.new_objects, objs...); layer)

"""

"""
function populate!(layer::Layer)
    union!(layer.objects, layer.new_objects)
    empty!(layer.new_objects)
    return layer
end

"""

"""
kill!(layer::Layer, objs...) = (push!(layer.dead_objects, objs...); layer)

"""

"""
function cleanup!(layer::Layer)
    setdiff!(layer.objects, layer.dead_objects)
    empty!(layer.dead_objects)
    return layer
end
