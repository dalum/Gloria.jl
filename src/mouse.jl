setrelativemousemode!(val::Bool) = SDL.SDL_SetRelativeMouseMode(SDL.SDL_bool(val))
showcursor!(show::Bool) = SDL.SDL_ShowCursor(Int32(show))

const MOUSE_PAIRS=(:type => UInt32, :timestamp => UInt32,
                        :windowID => UInt32, :which => UInt32, :state => UInt32)

function parseevent(window::Window, ::Val{UInt32(SDL.SDL_MOUSEMOTION)},data::Base.RefValue{SDL.SDL_Event})
    return geteventdata(:mousemotion, data[].motion, MOUSE_PAIRS...,
                        :x => Int32, :y => Int32, :xrel => Int32, :yrel => Int32)
end

function _parseevent(data::Base.RefValue{SDL.SDL_Event})
    return (data[].button, MOUSE_PAIRS..., :button => UInt8,:clicks => UInt8, :x => Int32, :y => Int32)
end
function parseevent(window::Window, ::Val{UInt32(SDL.SDL_MOUSEBUTTONUP)},data::Base.RefValue{SDL.SDL_Event})
    return geteventdata(:mousebutton_up, _parseevent(data)...)
end
function parseevent(window::Window, ::Val{UInt32(SDL.SDL_MOUSEBUTTONDOWN)},data::Base.RefValue{SDL.SDL_Event})
    return geteventdata(:mousebutton_down, _parseevent(data)...)
end

const BUTTONS = Dict{String,UInt8}("left" => 0x01, "middle" => 0x03, "right" => 0x03)

isbutton(e::Event, button::String) = e.button == BUTTONS[button]

# Mouse state

function getmousestate()
    x, y = Int[0], Int[0]
    mask = SDL.SDL_GetMouseState(pointer(x), pointer(y))
    return (x = x[], y = y[], left = (mask & 1 != 0)::Bool, middle = (mask & 2 != 0)::Bool, right = (mask & 4 != 0)::Bool)
end
