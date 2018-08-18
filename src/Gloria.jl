module Gloria

using SimpleDirectMediaLayer
const SDL = SimpleDirectMediaLayer

const _window_ptr = Ptr{SDL.Window}[0]
const _render_ptr = Ptr{SDL.Renderer}[0]
const _setup = Bool[false]
const _running = Bool[false]
const _loops = Set{Task}()

function init()
    SDL.GL_SetAttribute(SDL.GL_MULTISAMPLEBUFFERS, 16)
    SDL.GL_SetAttribute(SDL.GL_MULTISAMPLESAMPLES, 16)
    SDL.init()
end

function setup(title::String, width::Int, height::Int;
               fullscreen::Bool = false)
    if !_setup[]
        _window_ptr[] = SDL.CreateWindow(title,
                                                Int32(SDL.WINDOWPOS_CENTERED_MASK),
                                                Int32(SDL.WINDOWPOS_CENTERED_MASK),
                                                Int32(width), Int32(height),
                                                fullscreen ? SDL.WINDOW_FULLSCREEN : UInt32(0))
        _render_ptr[] = SDL.CreateRenderer(_window_ptr[], Int32(-1),
                                                  UInt32(SDL.RENDERER_ACCELERATED | SDL.RENDERER_PRESENTVSYNC))
        _setup[] = true
    else
        @info "Gloria has already been setup, ignoring"
    end

    return nothing
end

function quit()
    SDL.DestroyWindow(_window_ptr[])
    SDL.DestroyRenderer(_render_ptr[])
    _setup[] = false

    return nothing
end

function eventloop(fn::Function)
    if _running[]
        @error "Cannot setup event loop while running"
    else
        loop = @task begin
            while _running[]
                event = Array{UInt8}(zeros(56))
                while SDL.PollEvent(event) != 0
                    fn(event)
                end
                sleep(0)
            end
        end
        push!(_loops, loop)
    end
    return nothing
end

function updateloop(fn::Function; target_speed::Real = 100.0)
    if _running[]
        @error "Cannot setup update loop while running"
    else
        loop = @task begin
            t = 0.0
            dt = 1/target_speed
            while _running[]
                time_elapsed = @elapsed fn(t, dt)
                regression_factor = 1 - time_elapsed*target_speed
                dt = Int(2 - ceil(regression_factor)) / target_speed
                t += dt
                sleep(max(regression_factor/target_speed, 0.0))
            end
        end
        push!(_loops, loop)
    end
    return nothing
end

function renderloop(fn::Function; target_fps::Real = 60.0)
    if _running[]
        @error "Cannot setup render loop while running"
    else
        loop = @task begin
            frame = 1
            actual_fps = target_fps
            while _running[]
                t = @elapsed for _ in 1:ceil(target_fps)
                    time_elapsed = @elapsed fn(frame, actual_fps)
                    sleep(max(1/target_fps - time_elapsed, 0.0))
                end
                actual_fps = 1 / t
            end
        end
        push!(_loops, loop)
    end
    return nothing
end

function run()
    if _running[]
        @info "Already running, ignoring"
    else
        _running[] = true
        schedule.(_loops)
    end
end

function stop()
    if !_running[]
        @info "Already stopped, ignoring"
    else
        _running[] = false
        for loop in _loops
            wait(loop)
            pop!(_loops, loop)
        end
    end
end

include("graphics.jl")

end # module
