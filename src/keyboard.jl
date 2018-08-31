struct KeyboardState
    state::Array{UInt8}
    KeyboardState() = new(unsafe_wrap(Array{UInt8}, SDL.GetKeyboardState(C_NULL), 285))
end

ispressed(keyboard::KeyboardState, key::String) = keyboard.state[Gloria.SCANCODES[lowercase(key)] + 1] == 1
iskey(e::Event, key::String; repeat=false) = e.scancode == Gloria.SCANCODES[lowercase(key)] && convert(Bool, e.repeat) == repeat
iskeycode(e::Event, key::String) = e.keycode == Gloria.KEYCODES[lowercase(key)]

function parseevent(window::Window, ::Val{SDL.KEYDOWN}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :state => UInt8, :repeat => UInt8, :empty => UInt16, :scancode => UInt32,
                     :keycode => UInt32, :mod => UInt16)
    return (:key_down, e)
end

function parseevent(window::Window, ::Val{SDL.KEYUP}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :state => UInt8, :repeat => UInt8, :empty => UInt16, :scancode => UInt32,
                     :keycode => UInt32, :mod => UInt16)
    return (:key_up, e)
end

include("scancodes.jl")
include("keycodes.jl")
