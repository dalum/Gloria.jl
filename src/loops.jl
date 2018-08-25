# Default loops
function _eventloop(window::Window, target_speed::Float64)
    @task begin
        t = time()
        event_data = zeros(UInt8, 56)
        while length(window.scene_stack) > 0
            while SDL.PollEvent(event_data) != 0
                event_type, event = parseevent(window, event_data)
                onevent!(window, Val(event_type), event)
            end
            dt = time() - t
            t += dt
            sleep(max(1/target_speed - dt - 0.001, 0.0))
        end
    end
end

function _updateloop(window::Window, target_speed::Float64)
    @task begin
        t0 = time()
        t = t0
        dt = 1/target_speed
        while length(window.scene_stack) > 0
            update!(window, t=(t - t0), dt=dt)
            dt = time() - t
            t += dt
            sleep(max(1/target_speed - dt - 0.001, 0.0))
        end
    end
end

function _renderloop(window::Window, target_speed::Float64)
    @task begin
        t = t′ = time()
        frame = 1
        fps = 0.0
        while length(window.scene_stack) > 0
            render!(window, frame=frame, fps=fps)
            dt = time() - t
            t += dt
            frame += 1
            sleep(max(1/target_speed - dt - 0.001, 0.0))
            if frame % target_speed == 0
                fps = target_speed / (time() - t′)
                t′ = time()
            end
        end
    end
end

"""
run!(window::Window; args...)

Execute `window`.

"""
function run!(window::Window; target_event_speed::Float64 = 100.0, target_update_speed::Float64 = 30.0, target_render_speed::Float64 = 30.0, eventloop::Function = _eventloop, updateloop::Function = _updateloop, renderloop::Function = _renderloop)
    append!(window.tasks, [eventloop(window, target_event_speed), updateloop(window, target_update_speed), renderloop(window, target_render_speed)])
    schedule.(window.tasks)
    return window
end

function onevent!(::AbstractObject, ::Val, ::Event) end
function update!(::AbstractObject; args...) end
function render!(::Window, ::AbstractObject; args...) end

onevent!(window::Window, ::Val{:notsupported}, ::Event) = window
onevent!(window::Window, val::Val, e::Event) = onevent!(window.scene_stack[end], val, e)

struct InterruptQuit end
function onevent!(window::Window, val::Val{:quit}, e::Event)
    while length(window.scene_stack) > 0
        scene = pop!(window)
        if onevent!(scene, val, e) === InterruptQuit()
            push!(window, scene)
            return InterruptQuit()
        end
    end
    destroy!(window)
end

update!(window::Window; t::Float64, dt::Float64) = update!(window.scene_stack[end], t=t, dt=dt)
render!(window::Window; frame::Int, fps::Float64) = render!(window, window.scene_stack[end], frame=frame, fps=fps)

function onevent!(scene::Scene, val::Val, e::Event)
    for layer in scene.layers
        onevent!(layer, val, e)
    end
    return scene
end

function update!(scene::Scene; t::Float64, dt::Float64)
    for layer in scene.layers
        update!(layer, t=t, dt=dt)
    end
    return scene
end

function render!(window::Window, scene::Scene; frame::Int, fps::Float64)
    setcolor!(window, scene.color)
    clear!(window)
    for layer in scene.layers
        render!(window, layer, frame=frame, fps=fps)
    end
    present!(window)
end

function onevent!(layer::Layer, val::Val, e::Event)
    for obj in layer.objects
        onevent!(obj, val, e)
    end
    return layer
end

function update!(layer::Layer; t::Float64, dt::Float64)
    for obj in layer.objects
        update!(obj, t=t, dt=dt)
    end
    return layer
end

function render!(window::Window, layer::Layer; frame::Int, fps::Float64)
    if layer.show
        # sort!(layer.objects, by=layer.sortby)
        for obj in layer.objects
            render!(layer, obj, frame=frame, fps=fps)
        end

        for r in layer._render_tasks
            render!(window, r)
        end
        empty!(layer._render_tasks)
    end
    return window
end
