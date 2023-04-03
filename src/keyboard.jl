# Events
const KEY_PAIRS=(:type => UInt32, :timestamp => UInt32, :windowID => UInt32,
                        :state => UInt8, :repeat => UInt8, :unused => UInt16, :scancode => UInt32,
                        :sym => UInt32, :mod => UInt16)
function parseevent(window::Window, ::Val{UInt32(SDL.SDL_KEYDOWN)}, data::Base.RefValue{SDL.SDL_Event})
    return geteventdata(:key_down, data[].key,KEY_PAIRS...)
end

function parseevent(window::Window, ::Val{UInt32(SDL.SDL_KEYUP)}, data::Base.RefValue{SDL.SDL_Event})
       return geteventdata(:key_up, data[].key,KEY_PAIRS...)
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
iskeycode(e::Event, key::String; repeat=false) = e.sym == getkeycode(key) && (repeat || convert(Bool, e.repeat) == repeat)

getkeyname(e::Event) = unsafe_string(SDL.SDL_GetScancodeName( e.scancode))
getkeycodename(e::Event) = unsafe_string(SDL.SDL_GetKeyName( e.sym))
getscancode(key::String) = SDL.SDL_GetScancodeFromName(key)
getkeycode(key::String) = SDL.SDL_GetKeyFromName(key)

# Keyboard state

struct KeyboardState
    state::Base.ReinterpretArray{Bool, 1, UInt8, Array{UInt8, 1}}
    KeyboardState() = new(reinterpret(Bool, unsafe_wrap(Array{UInt8,1}, SDL.SDL_GetKeyboardState(C_NULL), 285)))
end

ispressed(keyboard::KeyboardState, key::String) = keyboard.state[getscancode(key) + 1]
