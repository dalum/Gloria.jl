# Events

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

"""
    iskey(e::Event, key::String; repeat=false)

Check if the event `e` is a key event with scancode corresponding to
`key`.  If the keyword argument `repeat` is true, this function also
returns true if the event was triggered by a key repeat (i. e., a held
key).

"""
iskey(e::Event, key::String; repeat=false) = e.scancode == getscancode(key) && (repeat || convert(Bool, e.repeat) == repeat)

"""
    iskeycode(e::Event, key::String; repeat=false)

Check if the event `e` is a key event with keycode corresponding to
`key`.  If the keyword argument `repeat` is true, this function also
returns true if the event was triggered by a key repeat (i. e., a held
key).

"""
iskeycode(e::Event, key::String; repeat=false) = e.keycode == getkeycode(key) && (repeat || convert(Bool, e.repeat) == repeat)

getkeyname(e::Event) = unsafe_string(SDL.GetScancodeName(convert(Int32, e.scancode)))
getkeycodename(e::Event) = unsafe_string(SDL.GetKeycodeName(convert(Int32, e.keycode)))
getscancode(key::String) = SDL.GetScancodeFromName(key)
getkeycode(key::String) = SDL.GetKeycodeFromName(key)

# Keyboard state

struct KeyboardState
    state::Base.ReinterpretArray{Bool, 1, UInt8, Array{UInt8, 1}}
    KeyboardState() = new(reinterpret(Bool, unsafe_wrap(Array{UInt8,1}, SDL.GetKeyboardState(C_NULL), 285)))
end

ispressed(keyboard::KeyboardState, key::String) = keyboard.state[getscancode(key) + 1]
