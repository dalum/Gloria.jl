macro loop(name, body)
    return _loop(name, body)
end

macro loop(name, initialize, body, finalize=:())
    return _loop(name, initialize, body, finalize)
end

_loop(name::String, body::Expr) = _loop(name, :(), body)
function _loop(name, initialize, body, finalize=:(), target_speed=50)
    return quote
        function (window::Window, target_speed=$target_speed)
            loop = Loop($name, target_speed)

            task = @task try
                @debug "Running $($name) on thread: $(Threads.threadid())"
                delta_times = CircularBuffer{Float64}(ceil(Int, target_speed/4))
                ticks = 0
                t0 = time()
                elapsed_time = time() - t0
                push!(delta_times, 1/target_speed)
                $initialize
                while length(window.scene_stack) > 0
                    $body
                    expected_time = ticks/target_speed
                    push!(delta_times, time() - (t0 + elapsed_time))
                    elapsed_time += delta_times[end]
                    sleep(max(1/target_speed + expected_time - elapsed_time, 1e-3))
                    ticks += 1
                end
                $finalize
            catch e
                @error "$(loop.name)" exception=(e, catch_backtrace())
                rethrow()
            end

            loop.task = task
            return loop
        end
    end |> esc
end

# Default loops

const DEFAULT_EVENT_LOOP = @loop "event" (event_data = zeros(UInt8, 56)) begin
    while SDL.PollEvent(event_data) != 0
        event = parseevent(window, event_data)
        onevent!(window, event)
    end
end

const DEFAULT_UPDATE_LOOP = @loop "update" (t = elapsed_time; dt = 1/target_speed) begin
    if !loop.paused
        t = elapsed_time
    end

    for event in window.event_queue
        onevent!(window, event, t, dt)
    end
    empty!(window.event_queue)

    sort!(window.timer_queue)
    while length(window.timer_queue) > 0 && window.timer_queue[end].t1 <= t
        pop!(window.timer_queue).fn()
    end

    if !loop.paused
        before_update!(window, t, dt)
        update!(window, t, dt)
        after_update!(window, t, dt)
    end
end

const DEFAULT_RENDER_LOOP = @loop "render" () begin
    fps = length(delta_times)/sum(delta_times)
    render!(window, ticks, fps)
end

"""
    run!(window::Window, [loop])

Execute `window`.

"""
function run!(window::Window)
    loops = map(
        loop -> loop(window),
        (DEFAULT_EVENT_LOOP, DEFAULT_UPDATE_LOOP, DEFAULT_RENDER_LOOP)
    )
    for loop in loops
        window.loops[loop.name] = loop
    end
    Threads.@spawn schedule(loops[1])
    Threads.@spawn schedule(loops[2])
    schedule(loops[3])

    return window
end

function run!(window::Window, loops...)
    for loop in map(loop -> loop(window), loops)
        window.loops[loop.name] = loop
        schedule(loop)
    end
    return window
end

function register_loops!(window::Window, loops...)
    for loop in map(loop -> loop(window), loops)
        window.loops[loop.name] = loop
    end
    return window
end

function reload!(window)
    wait(window)
    empty!(window.tasks)
    return run!(window, DEFAULT_EVENT_LOOP, DEFAULT_UPDATE_LOOP, DEFAULT_RENDER_LOOP)
end

settimer!(window::Window, t::Real, fn::Function) = push!(window.timer_queue, Timeout(t, fn))

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
    @eval function $fn(::AbstractObject, ::AbstractLayer, t, dt) end
end
function render!(::Window, ::AbstractObject, frame, fps) end
function render!(::AbstractLayer, ::AbstractObject, frame, fps) end

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
    @eval $fn(window::Window, t, dt) = length(window.scene_stack) > 0 ? $fn(activescene(window), window, t, dt) : nothing
end

render!(window::Window, frame, fps) = length(window.scene_stack) > 0 ? render!(window, activescene(window), frame, fps) : nothing

function onevent!(scene::AbstractScene, e::Event)
    for layer in scene.layers
        onevent!(layer, e)
    end
    return scene
end

function onevent!(scene::AbstractScene, e::Event, t, dt)
    for layer in scene.layers
        onevent!(layer, e, t, dt)
    end
    return scene
end


for fn in [:before_update!, :update!, :after_update!]
    @eval function $fn(scene::AbstractScene, ::Window, t, dt)
        for layer in scene.layers
            $fn(layer, scene, t, dt)
        end
        return scene
    end
end

function render!(window::Window, scene::AbstractScene, frame, fps)
    setcolor!(window, scene.color)
    clear!(window)
    for layer in scene.layers
        render!(window, layer, frame, fps)
    end
    present!(window)
end

function onevent!(layer::AbstractLayer, e::Event)
    for obj in layer.objects
        onevent!(obj, e)
    end
    return layer
end

function onevent!(layer::AbstractLayer, e::Event, t, dt)
    for obj in layer.objects
        onevent!(obj, e, t, dt)
    end
    return layer
end

function before_update!(layer::AbstractLayer, ::AbstractScene, t, dt)
    populate!(layer)
    for obj in layer.objects
        before_update!(obj, layer, t, dt)
    end
    return layer
end

function update!(layer::AbstractLayer, ::AbstractScene, t, dt)
    for obj in layer.objects
        update!(obj, layer, t, dt)
    end
    return layer
end

function after_update!(layer::AbstractLayer, ::AbstractScene, t, dt)
    for obj in layer.objects
        after_update!(obj, layer, t, dt)
    end
    cleanup!(layer)
    return layer
end

function render!(window::Window, layer::AbstractLayer, frame, fps)
    if visible(layer)
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
