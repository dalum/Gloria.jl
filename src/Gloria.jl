module Gloria

using SimpleDirectMediaLayer
const SDL = SimpleDirectMediaLayer

using Colors

abstract type AbstractObject end
abstract type AbstractResource end
abstract type AbstractScene end

"""

A Scene object runs inside a Window.  Only one scene can be active
inside the window at any time.

"""
mutable struct Scene{T <: AbstractObject} <: AbstractScene
    camera_x::Int
    camera_y::Int
    color::Colors.RGB
    objects::Vector{T}
end

Scene() = Scene(AbstractObject[])
Scene{T}() where {T <: AbstractObject} = Scene(T[])
Scene(objects::Vector{T}) where {T <: AbstractObject} = Scene(0, 0, objects)
Scene(camera_x::Int, camera_y::Int, objects::Vector{T}) where {T <: AbstractObject} = Scene(camera_x, camera_y, Colors.colorant"dark gray", objects)

"""
push!(scene::AbstractScene, objs::AbstractObject...)

Add one or more `objs` to `scene`.

"""
Base.push!(scene::AbstractScene, obj::AbstractObject, objs::AbstractObject...) = push!(scene.objects, obj, objs...)

"""
append!(scene::AbstractScene, objs::AbstractVector{<:AbstractObject})

Add the objects in `objs` to `scene`.

"""
Base.append!(scene::AbstractScene, objs::AbstractVector{<:AbstractObject}) = append!(scene.objects, objs)

"""

A Window is the main GUI element.

"""
mutable struct Window{T <: AbstractScene}
    window_ptr::Ptr{SDL.Window}
    render_ptr::Ptr{SDL.Renderer}
    scene_stack::Vector{T}
    resources::Dict{String, AbstractResource}
end

"""

Create a Window with a title, size, and base scene.

"""
Window(args...; kwargs...) = Window{AbstractScene}(args...; kwargs...)
function Window{T}(title::String, width::Int, height::Int; target_fps::Number = 30.0, target_speed::Number = 30.0, fullscreen::Bool = false, show_cursor::Bool = true) where {T <: AbstractScene}
    window_ptr = SDL.CreateWindow(title, Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(SDL.WINDOWPOS_CENTERED_MASK), Int32(width), Int32(height), fullscreen ? SDL.WINDOW_FULLSCREEN : UInt32(0))
    render_ptr = SDL.CreateRenderer(window_ptr, Int32(-1), UInt32(SDL.RENDERER_ACCELERATED | SDL.RENDERER_PRESENTVSYNC | SDL.RENDERER_TARGETTEXTURE))
    SDL.SetRenderDrawBlendMode(render_ptr, SDL.BLENDMODE_BLEND)
    scene_stack = T[]
    resources = Dict{String,AbstractResource}()
    SDL.ShowCursor(Int32(show_cursor))
    window = Window(window_ptr, render_ptr, scene_stack, resources)
    finalizer(destroy!, window)
    push!(_windows, window)

    # Update loop
    function updateloop()
        t0 = time()
        t = 0.0
        dt = 1/target_speed
        event_data = zeros(UInt8, 56)
        while length(scene_stack) > 0
            while SDL.PollEvent(event_data) != 0
                event_type, event = Events.parseevent(window, event_data)
                onevent!(scene_stack[end], Val(event_type), event)
            end
            update!(scene_stack[end], t=t, dt=dt)
            t1 = time()
            dt = t1 - t0
            t0 = t1
            t += dt
            sleep(max(1/target_speed - dt - 0.001, 0.0))
        end
    end
    push!(_loops, Task(updateloop))

    # Render loop
    function renderloop()
        t0 = time()
        frame = 1
        actual_fps = target_fps
        while length(scene_stack) > 0
            t = time()
            for _ in 1:ceil(target_fps)
                render!(window, scene_stack[end], frame=frame, fps=actual_fps)
                t1 = time()
                time_elapsed = time() - t0
                t0 = t1
                frame += 1
                sleep(max(1/target_fps - time_elapsed - 0.001, 0.0))
            end
            actual_fps = target_fps / (time() - t)
        end
        return nothing
    end
    push!(_loops, Task(renderloop))

    return window
end

function destroy!(window::Window)
    SDL.DestroyWindow(window.window_ptr)
    SDL.DestroyRenderer(window.render_ptr)
    return nothing
end

function Base.getproperty(window::Window, name::Symbol)
    name == :width && return size(window)[1]
    name == :height && return size(window)[2]
    getfield(window, name)
end

const _windows = Window[]
const _loops = Task[]

"""

"""
function init(; multisampling = true)
    if multisampling
        SDL.GL_SetAttribute(SDL.GL_MULTISAMPLEBUFFERS, 1)
        SDL.GL_SetAttribute(SDL.GL_MULTISAMPLESAMPLES, 4)
    end
    SDL.init()
end

"""

"""
function start(;keepalive = false)
    schedule.(_loops)
    if keepalive
        while !all(istaskdone, _loops)
            sleep(0.1)
        end
    end
end

"""
push!(window::Window, scenes::AbstractScene...)

Add one or more `scenes` to the top of the scene stack of `window`.

"""
Base.push!(window::Window, scene::AbstractScene, scenes::AbstractScene...) = push!(window.scene_stack, scene, scenes...)

"""
append!(window::Window, scenes::AbstractVector{<:AbstractScene})

Add the elements of `scenes` to the the top of the scene stack of
`window`.

"""
Base.append!(window::Scene, scenes::AbstractVector{<:AbstractScene}) = append!(window.scene_stack, scenes)


"""
size(window::Window)

Return a tuple containing the dimensions of `window`.

"""
function Base.size(window::Window)
    width, height = Int32[0], Int32[0]
    SDL.GetWindowSize(window.window_ptr, pointer(width), pointer(height))
    return width[], height[]
end

include("events.jl")

function onevent!(::AbstractObject, ::Val, ::Events.Event) end
function update!(::AbstractObject; args...) end
function render!(::Window, ::AbstractObject; args...) end

function onevent!(::Scene, ::Val{:notsupported}, ::Events.Event) end
function onevent!(scene::Scene, val::Val, e::Events.Event)
    for obj in scene.objects
        onevent!(obj, val, e)
    end
end

function update!(scene::Scene; t::Float64, dt::Float64)
    for obj in scene.objects
        update!(obj, t=t, dt=dt)
    end
end

function render!(window::Window, scene::Scene; frame::Int, fps::Float64)
    Graphics.setcolor!(window, scene.color)
    Graphics.clear!(window)
    for obj in scene.objects
        render!(window, obj, frame=frame, fps=fps)
    end
    Graphics.present!(window)
end

include("graphics.jl")
include("mouse.jl")

end # module
