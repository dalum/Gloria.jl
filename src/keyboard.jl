function unsafe_getkeyboardstate()
    return unsafe_wrap(Array, SDL.GetKeyboardState(C_NULL), 285)
end

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
