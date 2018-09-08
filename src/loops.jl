const SLEEP_MAKEUP_TIME = 0.0011 # TODO: Calculate this automatically
const GLOBAL_LOOP_LOCK = [Channel{UInt8}(1)]
put!(GLOBAL_LOOP_LOCK[], 0x01)

macro loop(name::String, body::Expr)
    _loop(name, body)
end

macro loop(name::String, initialize::Expr, body::Expr, finalize::Expr = :())
    _loop(name, initialize, body, finalize)
end

_loop(name::String, body::Expr) = _loop(name, :(), body)
function _loop(name::String, initialize::Expr, body::Expr, finalize::Expr = :())
    global GLOBAL_LOOP_LOCK
    quote
        function (window::Window, target_speed::Float64)
            _lock = 0x00
            @task try
                $initialize
                while length(window.scene_stack) > 0
                    t1 = time()
                    _lock = take!($GLOBAL_LOOP_LOCK[])
                    $body
                    put!($GLOBAL_LOOP_LOCK[], _lock)
                    _lock = 0x00
                    t2 = time()
                    sleep(max(1/target_speed - (t2 - t1) - $SLEEP_MAKEUP_TIME, 0.0))
                end
                $finalize
            catch e
                if !(e isa InvalidStateException)
                    println("ERROR ($($name)): ", sprint(showerror, e, stacktrace(catch_backtrace())))
                    _lock === 0x01 && put!($GLOBAL_LOOP_LOCK[], _lock)
                end
            end
        end
    end
end

# Default loops

const DEFAULT_EVENT_LOOP = @loop "event loop" (event_data = zeros(UInt8, 56)) while SDL.PollEvent(event_data) != 0
    event = parseevent(window, event_data)
    onevent!(window, event)
end

const DEFAULT_UPDATE_LOOP = @loop "update loop" (t0 = time(); t = 0.0; dt = 0.0) begin
    dt = min(t1 - t0, 2/target_speed)
    t0 = t1
    t += dt
    before_update!(window, t=t, dt=dt)
    update!(window, t=t, dt=dt)
    after_update!(window, t=t, dt=dt)
end

const DEFAULT_RENDER_LOOP = @loop "render loop" (frame = 1; fps = 0.0; t0 = time()) begin
    if frame % target_speed == 0
        fps = target_speed / (time() - t0)
        t0 = time()
    end
    render!(window, frame=frame, fps=fps)
    frame += 1
end

"""
    run!(window::Window, [loop])

Execute `window`.

"""
function run!(window::Window; target_event_speed::Float64 = 100.0, target_update_speed::Float64 = 60.0, target_render_speed::Float64 = 60.0)
    return run!(window,
                target_event_speed => DEFAULT_EVENT_LOOP,
                target_update_speed => DEFAULT_UPDATE_LOOP,
                target_render_speed => DEFAULT_RENDER_LOOP)
end

function run!(window::Window, loops::Pair{Float64,<:Function}...)
    tasks = [loop(window, speed) for (speed, loop) in loops]
    append!(window.tasks, tasks)
    schedule.(tasks)
    return window
end

function reload!(window; target_event_speed::Float64 = 100.0, target_update_speed::Float64 = 60.0, target_render_speed::Float64 = 60.0)
    close(GLOBAL_LOOP_LOCK[])
    wait(window)
    empty!(window.tasks)
    GLOBAL_LOOP_LOCK[] = Channel{UInt8}(1)
    put!(GLOBAL_LOOP_LOCK[], 0x01)
    return run!(window,
                target_event_speed => DEFAULT_EVENT_LOOP,
                target_update_speed => DEFAULT_UPDATE_LOOP,
                target_render_speed => DEFAULT_RENDER_LOOP)
end

struct InterruptQuit end

"""
    quit!(window::Window, e::Event)

Gracefully quit `window`, propagating the event, `e`, which caused
`quit!` to be called to each scene in `window` to allow processing and
optionally interrupting the call by returning `InterruptQuit()`.

"""
function quit!(window::Window, e::Event)
    while length(window.scene_stack) > 0
        scene = pop!(window)
        if onevent!(scene, e) === InterruptQuit()
            push!(window, scene)
            return InterruptQuit()
        end
    end
    destroy!(window)
end

function onevent!(::AbstractObject, ::Event) end
for fn in [:before_update!, :update!, :after_update!]
    @eval function $fn(::AbstractObject; t=t, dt=dt) end
end
function render!(::Window, ::AbstractObject; args...) end
function render!(::Layer, ::AbstractObject; args...) end

onevent!(window::Window, ::Event{:notsupported}) = window
onevent!(window::Window, e::Event) = length(window.scene_stack) > 0 ? onevent!(window.scene_stack[end], e) : nothing
onevent!(window::Window, e::Event{:quit}) = quit!(window, e)

for fn in [:before_update!, :update!, :after_update!]
    @eval $fn(window::Window; t::Float64, dt::Float64) = length(window.scene_stack) > 0 ? $fn(window.scene_stack[end], t=t, dt=dt) : nothing
end

render!(window::Window; frame::Int, fps::Float64) = length(window.scene_stack) > 0 ? render!(window, window.scene_stack[end], frame=frame, fps=fps) : nothing

function onevent!(scene::Scene, e::Event)
    for layer in scene.layers
        onevent!(layer, e)
    end
    return scene
end

for fn in [:before_update!, :update!, :after_update!]
    @eval function $fn(scene::Scene; t::Float64, dt::Float64)
        for layer in scene.layers
            $fn(layer, t=t, dt=dt)
        end
        return scene
    end
end

function render!(window::Window, scene::Scene; frame::Int, fps::Float64)
    setcolor!(window, scene.color)
    clear!(window)
    for layer in scene.layers
        render!(window, layer, frame=frame, fps=fps)
    end
    present!(window)
end

function onevent!(layer::Layer, e::Event)
    for obj in layer.objects
        onevent!(obj, e)
    end
    return layer
end

function before_update!(layer::Layer; t::Float64, dt::Float64)
    populate!(layer)
    for obj in layer.objects
        before_update!(obj, t=t, dt=dt)
    end
    return layer
end

function update!(layer::Layer; t::Float64, dt::Float64)
    for obj in layer.objects
        update!(obj, t=t, dt=dt)
    end
    return layer
end

function after_update!(layer::Layer; t::Float64, dt::Float64)
    for obj in layer.objects
        after_update!(obj, t=t, dt=dt)
    end
    cleanup!(layer)
    return layer
end

function render!(window::Window, layer::Layer; frame::Int, fps::Float64)
    if layer.show
        # sort!(layer.objects, by=layer.sortby)
        for obj in layer.objects
            render!(layer, obj, frame=frame, fps=fps)
        end

        for r in layer.render_tasks
            render!(window, layer, r)
        end
        empty!(layer.render_tasks)
    end
    return window
end
