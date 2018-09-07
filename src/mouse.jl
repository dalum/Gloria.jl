setrelativemousemode!(val::Bool) = SDL.SetRelativeMouseMode(val)
showcursor!(show::Bool) = SDL.ShowCursor(Int32(show))

function parseevent(window::Window, ::Val{SDL.MOUSEMOTION}, data::Vector{UInt8})
    return geteventdata(:mousemotion, data, :type => UInt32, :timestamp => UInt32,
                        :window_id => UInt32, :which => UInt32, :state => UInt32,
                        :x => Int32, :y => Int32, :rel_x => Int32, :rel_y => Int32)
end

function parseevent(window::Window, ::Val{SDL.MOUSEBUTTONDOWN}, data::Vector{UInt8})
    return geteventdata(:mousebutton_down, data, :type => UInt32, :timestamp => UInt32,
                        :window_id => UInt32, :which => UInt32, :button => UInt8, :state => UInt8,
                        :clicks => UInt8, :unused => UInt8, :x => Int32, :y => Int32)
end

function parseevent(window::Window, ::Val{SDL.MOUSEBUTTONUP}, data::Vector{UInt8})
    return geteventdata(:mousebutton_up, data, :type => UInt32, :timestamp => UInt32,
                        :window_id => UInt32, :which => UInt32, :button => UInt8, :state => UInt8,
                        :clicks => UInt8, :unused => UInt8, :x => Int32, :y => Int32)
end

const BUTTONS = Dict{String,UInt8}("left" => 0x01, "middle" => 0x03, "right" => 0x03)

isbutton(e::Event, button::String) = e.button == BUTTONS[button]

# Mouse state

function getmousestate()
    x, y = Int[0], Int[0]
    mask = SDL.GetMouseState(pointer(x), pointer(y))
    return (x = x[], y = y[], left = (mask & 1 != 0)::Bool, middle = (mask & 2 != 0)::Bool, right = (mask & 4 != 0)::Bool)
end
