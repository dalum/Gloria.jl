abstract type AbstractScene{T} end
abstract type AbstractLayer{T} end
abstract type AbstractWindow{T} end

struct Event{TYPE}
    fields::Dict{Symbol, Any}
end
Event{TYPE}() where {TYPE} = Event{TYPE}(Dict{Symbol, Any}())
Event{TYPE}(pairs::Pair...) where {TYPE}  = Event{TYPE}(Dict{Symbol, Any}(pairs...))

Base.getproperty(e::Event, name::Symbol) = getfield(e, :fields)[name]
Base.setproperty!(e::Event, name::Symbol, x) = setindex!(getfield(e, :fields), x, name)

mutable struct Loop
    name::String
    target_speed::Float64
    paused::Bool
    task::Task
    Loop(name::String, target_speed=50.0) = new(name, target_speed, false)
end

Base.schedule(loop::Loop) = schedule(loop.task)
Base.wait(loop::Loop) = wait(loop.task)

struct Timeout
    t1::Float64
    fn::Function
end
Base.isless(a::Timeout, b::Timeout) = isless(b.t1, a.t1)

"""

A Window.

"""
mutable struct Window{T} <: AbstractWindow{T}
    window_ptr::Ptr{SDL.Window}
    render_ptr::Ptr{SDL.Renderer}
    scene_stack::T
    loops::Dict{String, Loop}
    event_queue::Vector{Event}
    timer_queue::Vector{Timeout}

    function Window{T}(title, width, height, scene_stack::T; fullscreen=false) where {T}
        window_ptr = SDL.CreateWindow(title, Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(width), Int32(height), fullscreen ? SDL.WINDOW_FULLSCREEN : UInt32(0))
        render_ptr = SDL.CreateRenderer(window_ptr, Int32(-1), UInt32(0))
        if render_ptr == C_NULL
            SDL.DestroyWindow(window_ptr)
            SDL.DestroyRenderer(render_ptr)
            error(unsafe_string(SDL.GetError()))
        end
        SDL.SetRenderDrawBlendMode(render_ptr, SDL.BLENDMODE_BLEND)
        loops = Dict{String,Loop}()
        event_queue = Event[]
        timer_queue = Timeout[]
        self = new(window_ptr, render_ptr, scene_stack, loops, event_queue, timer_queue)
        finalizer(destroy!, self)
        return self
    end
end

Window(title, width, height; kwargs...) = Window(title, width, height, AbstractScene[]; kwargs...)
function Window(title, width, height, scene_stack::T; fullscreen=false) where {T}
    return Window{T}(title, width, height, scene_stack, fullscreen=fullscreen)
end

function Window(title, width, height, scenes::AbstractScene...; kwargs...)
    return Window(title, width, height, [scenes...]; kwargs...)
end

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

activescene(window::Window) = window.scene_stack[end]

"""

`Scene` objects run inside a `Window`.  Only one scene can be active
inside a given `Window` at any time.

"""
mutable struct Scene{T <: AbstractVector{<:AbstractLayer}} <: AbstractScene{T}
    layers::T
    color::Colors.RGB
end

Scene(layer::AbstractLayer, layers::AbstractLayer...; color=Colors.colorant"darkgray") = Scene([layer, layers...], color)
Scene(layers::T) where {T<:AbstractVector{<:AbstractLayer}} = Scene(layers, Colors.colorant"darkgray")

struct RenderTask{T}
    source::T
    x::Float64
    y::Float64
    θ::Float64
    scale::Float64
    offset_x::Int
    offset_y::Int
    flip::Symbol
    color::Colors.RGB
end

RenderTask(source::T, args...) where {T} = RenderTask{T}(source, args...)

"""

A Layer object runs inside a Scene.

"""
mutable struct Layer{T} <: AbstractLayer{T}
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

visible(layer::Layer) = layer.show

Base.show(io::IO, layer::AbstractLayer) = print(io, "Layer(", join([layer.objects, layer.x, layer.y, layer.θ], ", "), ")")

Base.iterate(layer::AbstractLayer) = iterate(layer.objects)
Base.iterate(layer::AbstractLayer, state) = iterate(layer.objects, state)
Base.filter(f, layer::AbstractLayer) = filter(f, layer.objects)

"""
    wait(window::Window)

Block the current task until `window` has finished executing.

"""
function Base.wait(window::Window)
    for (name, loop) in window.loops
        wait(loop)
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
Base.push!(layer::AbstractLayer, objs...) = (push!(layer.objects, objs...); layer)
Base.delete!(layer::AbstractLayer, obj) = (delete!(layer.objects, obj); layer)

Base.getindex(layer::Layer, index) = getindex(layer.objects, index)

isalive(layer::Layer, obj) = (obj in layer.objects && !(obj in layer.dead_objects))

"""

"""
add!(layer::AbstractLayer, objs...) = (push!(layer.new_objects, objs...); layer)

"""

"""
function populate!(layer::AbstractLayer)
    union!(layer.objects, layer.new_objects)
    empty!(layer.new_objects)
    return layer
end

"""

"""
kill!(layer::AbstractLayer, objs...) = (push!(layer.dead_objects, objs...); layer)
killall!(layer::AbstractLayer) = (union!(layer.dead_objects, layer.objects); layer)

"""

"""
function cleanup!(layer::AbstractLayer)
    setdiff!(layer.objects, layer.dead_objects)
    empty!(layer.dead_objects)
    return layer
end

tocoordinates(layer::Layer, x, y) = x - layer.x, y - layer.y
