setrelativemousemode!(val::Bool) = SDL.SetRelativeMouseMode(val)
showcursor!(show::Bool) = SDL.ShowCursor(Int32(show))

function getmousestate()
    x, y = Int[0], Int[0]
    mask = SDL.GetMouseState(pointer(x), pointer(y))
    return (x = x[], y = y[], left = (mask & 1 != 0)::Bool, middle = (mask & 2 != 0)::Bool, right = (mask & 4 != 0)::Bool)
end

function parseevent(window::Window, ::Val{SDL.MOUSEMOTION}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :which => UInt32, :state => UInt32, :x => Int32, :y => Int32,
                     :rel_x => Int32, :rel_y => Int32)
    return (:mousemotion, e)
end

function parseevent(window::Window, ::Val{SDL.MOUSEBUTTONDOWN}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :which => UInt32, :button => UInt8, :state => UInt8, :clicks => UInt8,
                     :x => Int32, :y => Int32)
    return (:mousebutton_down, e)
end

function parseevent(window::Window, ::Val{SDL.MOUSEBUTTONUP}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :which => UInt32, :button => UInt8, :state => UInt8, :clicks => UInt8,
                     :x => Int32, :y => Int32)
    return (:mousebutton_up, e)
end
