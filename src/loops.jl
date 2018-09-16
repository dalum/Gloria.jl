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
function _loop(name::String, initialize::Expr, body::Expr, finalize::Expr = :(), target_speed::Float64 = 50.0)
    global GLOBAL_LOOP_LOCK
    quote
        function (window::Window, target_speed::Float64 = $target_speed)
            loop = Loop($name, target_speed)
            _lock = 0x00

            task = @task try
                state = loop.state
                $initialize
                while length(window.scene_stack) > 0
                    t1 = time()
                    _lock = take!($GLOBAL_LOOP_LOCK[])
                    $body
                    put!($GLOBAL_LOOP_LOCK[], _lock)
                    _lock = 0x00
                    t2 = time()
                    sleep(max(1/loop.target_speed - (t2 - t1) - $SLEEP_MAKEUP_TIME, 0.0))
                end
                $finalize
            catch e
                if !(e isa InvalidStateException)
                    println("ERROR ($(loop.name)): ", sprint(showerror, e, stacktrace(catch_backtrace())))
                    _lock === 0x01 && put!($GLOBAL_LOOP_LOCK[], _lock)
                end
            end

            loop.task = task
            return loop
        end
    end
end

# Default loops

const DEFAULT_EVENT_LOOP = @loop "event" (event_data = zeros(UInt8, 56)) begin
    while SDL.PollEvent(event_data) != 0
        event = parseevent(window, event_data)
        onevent!(window, event)
    end
end

const DEFAULT_UPDATE_LOOP = @loop "update" (state[:t0] = time(); state[:t] = 0.0; state[:dt] = 0.0) begin
    state[:dt] = min(t1 - state[:t0], 5/target_speed)
    state[:t0] = t1
    state[:t] += state[:dt]

    for event in window.event_queue
        onevent!(window, event, state[:t], state[:dt])
    end
    empty!(window.event_queue)

    sort!(window.timer_queue)
    while length(window.timer_queue) > 0 && window.timer_queue[end].t1 <= state[:t]
        pop!(window.timer_queue).fn()
    end

    before_update!(window, state[:t], state[:dt])
    update!(window, state[:t], state[:dt])
    after_update!(window, state[:t], state[:dt])
end

const DEFAULT_RENDER_LOOP = @loop "render" (state[:frame] = 1; fps = 0.0; state[:t0] = time()) begin
    if state[:frame] % target_speed == 0
        fps = target_speed / (time() - state[:t0])
        state[:t0] = time()
    end
    render!(window, state[:frame], fps)
    state[:frame] += 1
end

"""
    run!(window::Window, [loop])

Execute `window`.

"""
function run!(window::Window)
    return run!(window, DEFAULT_EVENT_LOOP, DEFAULT_UPDATE_LOOP, DEFAULT_RENDER_LOOP)
end

function run!(window::Window, loops::Function...)
    for loop in map(loop->loop(window), loops)
        window.loops[loop.name] = loop
        schedule(loop)
    end
    return window
end

function reload!(window; target_event_speed::Float64 = 50.0, target_update_speed::Float64 = 50.0, target_render_speed::Float64 = 50.0)
    close(GLOBAL_LOOP_LOCK[])
    wait(window)
    empty!(window.tasks)
    GLOBAL_LOOP_LOCK[] = Channel{UInt8}(1)
    put!(GLOBAL_LOOP_LOCK[], 0x01)
    return run!(window, DEFAULT_EVENT_LOOP, DEFAULT_UPDATE_LOOP, DEFAULT_RENDER_LOOP)
end

settimer!(window::Window, t::Real, fn::Function) = push!(window.timer_queue, Timeout(convert(Float64, t), fn))

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
function onevent!(::AbstractObject, ::Event, t, dt) end
for fn in [:before_update!, :update!, :after_update!]
    @eval function $fn(::AbstractObject, t, dt) end
end
function update!(::AbstractObject, ::AbstractObject, t, dt) end
function render!(::Window, ::AbstractObject, frame, fps) end
function render!(::Layer, ::AbstractObject, frame, fps) end

onevent!(window::Window, ::Event{:notsupported}) = window
function onevent!(window::Window, e::Event)
    if length(window.scene_stack) > 0
        onevent!(activescene(window), e)
    end
    push!(window.event_queue, e)
    return window
end
onevent!(window::Window, e::Event{:quit}) = quit!(window, e)

onevent!(window::Window, e::Event, t, dt) = length(window.scene_stack) > 0 ? onevent!(activescene(window), e, t, dt) : window

for fn in [:before_update!, :update!, :after_update!]
    @eval $fn(window::Window, t::Float64, dt::Float64) = length(window.scene_stack) > 0 ? $fn(activescene(window), t, dt) : nothing
end

render!(window::Window, frame::Int, fps::Float64) = length(window.scene_stack) > 0 ? render!(window, activescene(window), frame, fps) : nothing

function onevent!(scene::Scene, e::Event)
    for layer in scene.layers
        onevent!(layer, e)
    end
    return scene
end

function onevent!(scene::Scene, e::Event, t, dt)
    for layer in scene.layers
        onevent!(layer, e, t, dt)
    end
    return scene
end


for fn in [:before_update!, :update!, :after_update!]
    @eval function $fn(scene::Scene, t::Float64, dt::Float64)
        for layer in scene.layers
            $fn(layer, t, dt)
        end
        return scene
    end
end

function render!(window::Window, scene::Scene, frame::Int, fps::Float64)
    setcolor!(window, scene.color)
    clear!(window)
    for layer in scene.layers
        render!(window, layer, frame, fps)
    end
    present!(window)
end

function onevent!(layer::Layer, e::Event)
    for obj in layer.objects
        onevent!(obj, e)
    end
    return layer
end

function onevent!(layer::Layer, e::Event, t, dt)
    for obj in layer.objects
        onevent!(obj, e, t, dt)
    end
    return layer
end

function before_update!(layer::Layer, t::Float64, dt::Float64)
    populate!(layer)
    for obj in layer.objects
        before_update!(obj, t, dt)
    end
    return layer
end

function update!(layer::Layer, t::Float64, dt::Float64)
    for obj1 in layer.objects, obj2 in layer.objects
        if obj1 === obj2
            update!(obj1, t, dt)
        else
            update!(obj1, obj2, t, dt)
        end
    end
    return layer
end

function after_update!(layer::Layer, t::Float64, dt::Float64)
    for obj in layer.objects
        after_update!(obj, t, dt)
    end
    cleanup!(layer)
    return layer
end

function render!(window::Window, layer::Layer, frame::Int, fps::Float64)
    if layer.show
        # sort!(layer.objects, by=layer.sortby)
        for obj in layer.objects
            render!(layer, obj, frame, fps)
        end

        for r in layer.render_tasks
            render!(window, layer, r)
        end
        empty!(layer.render_tasks)
    end
    return window
end
