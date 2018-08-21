module Gloria

using SimpleDirectMediaLayer
const SDL = SimpleDirectMediaLayer

abstract type AbstractWindow end
abstract type AbstractScene end
abstract type AbstractObject end
abstract type AbstractResource end

"""

A Window is the main GUI element.

"""
mutable struct Window <: AbstractWindow
    window_ptr::Ptr{SDL.Window}
    render_ptr::Ptr{SDL.Renderer}
    scene_stack::Vector{<:AbstractScene}
    resources::Dict{String, AbstractResource}
end

"""

A Scene object runs inside a Window.  Only one scene can be active
inside the window at any time.

"""
mutable struct Scene <: AbstractScene
    objects::Vector{AbstractObject}
end

const _windows = AbstractWindow[]
const _loops = Task[]

function init()
    # SDL.GL_SetAttribute(SDL.GL_MULTISAMPLEBUFFERS, 16)
    # SDL.GL_SetAttribute(SDL.GL_MULTISAMPLESAMPLES, 16)
    SDL.init()
end

function Base.size(window::Window)
    width, height = Int32[0], Int32[0]
    SDL.GetWindowSize(window.window_ptr, pointer(width), pointer(height))
    return width[], height[]
end

"""

Create a Window with a title, size, and initial scene.

"""
Window(title::String, (width, height)::Tuple{Int,Int}; kwargs...) = Window(title, (width, height), Scene(AbstractObject[]); kwargs...)

function Window(title::String, (width, height)::Tuple{Int,Int}, initial_scene::AbstractScene;
                target_fps::Number = 30.0, target_speed::Number = 50.0, fullscreen::Bool = false)
    window_ptr = SDL.CreateWindow(title,
                                  Int32(SDL.WINDOWPOS_CENTERED_MASK),
                                  Int32(SDL.WINDOWPOS_CENTERED_MASK),
                                  Int32(width), Int32(height),
                                  fullscreen ? SDL.WINDOW_FULLSCREEN : UInt32(0))
    render_ptr = SDL.CreateRenderer(window_ptr, Int32(-1),
                                    UInt32(SDL.RENDERER_ACCELERATED | SDL.RENDERER_PRESENTVSYNC))
    SDL.SetRenderDrawBlendMode(render_ptr, SDL.BLENDMODE_BLEND)
    scene_stack = AbstractScene[initial_scene]
    resources = Dict{String,AbstractResource}()

    window = Window(window_ptr, render_ptr, scene_stack, resources)
    finalizer(destroy!, window)
    push!(_windows, window)

    # Update loop
    function updateloop()
        t0 = time()
        t = 0
        dt = 1/target_speed
        while length(scene_stack) > 0
            event = Event()
            while SDL.PollEvent(event.data) != 0
                handleevent(Val(eventtype(event)), event)
            end
            for obj in scene_stack[end].objects
                update!(obj, t=t, dt=dt)
            end
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
                for obj in scene_stack[end].objects
                    render!(_windows[end], obj, frame=frame, fps=actual_fps)
                end
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

    schedule.(_loops)

    return window
end

function destroy!(window::Window)
    SDL.DestroyWindow(window.window_ptr)
    SDL.DestroyRenderer(window.render_ptr)
    return nothing
end

update!(obj::AbstractObject; kwargs...) = nothing
render!(window::Window, obj::AbstractObject; kwargs...) = nothing

# Events

struct Event
    data::Array{UInt8}
end
Event() = Event(zeros(56))

function bitcat(::Type{T}, arr)::T where T<:Number
    out = zero(T)
    for x in arr
        out <<= sizeof(x)*8
        out |= convert(T, x)
    end
    out
end

eventtype(e::Event) = bitcat(UInt32, e.data[4:-1:1])

handleevent(::Val, e::Event) = nothing
function handleevent(::Val{SDL.QUIT}, e::Event)
    println("Quit!")
end

include("graphics.jl")
include("mouse.jl")

end # module
