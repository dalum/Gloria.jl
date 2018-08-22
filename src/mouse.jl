module Mouse

import Gloria.Events: parseevent
using Gloria: SDL, Window
using Gloria.Events: geteventdata
using Gloria.Graphics: transformview, transformrelative

setrelative(val::Bool) = SDL.SetRelativeMouseMode(val)

function getmousestate()
    x, y = Int[0], Int[0]
    mask = SDL.GetMouseState(pointer(x), pointer(y))
    return (x = x[], y = y[], left = (mask & SDL.BUTTON_LEFT != 0)::Bool, middle = (mask & SDL.BUTTON_MIDDLE != 0)::Bool, right = (mask & SDL.BUTTON_RIGHT != 0)::Bool)
end

function parseevent(window::Window, ::Val{SDL.MOUSEMOTION}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :which => UInt32, :state => UInt32, :x => Int32, :y => Int32,
                     :rel_x => Int32, :rel_y => Int32)
    e.x, e.y = transformview(window, window.scene_stack[end], e.x, e.y)
    e.rel_x, e.rel_y = transformrelative(window, window.scene_stack[end], e.rel_x, e.rel_y)
    return (:mousemotion, e)
end

function parseevent(window::Window, ::Val{SDL.MOUSEBUTTONDOWN}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :which => UInt32, :button => UInt8, :state => UInt8, :clicks => UInt8,
                     :x => Int32, :y => Int32)
    e.x, e.y = transformview(window, window.scene_stack[end], e.x, e.y)
    return (:mousebutton_down, e)
end

function parseevent(window::Window, ::Val{SDL.MOUSEBUTTONUP}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32, :window_id => UInt32,
                     :which => UInt32, :button => UInt8, :state => UInt8, :clicks => UInt8,
                     :x => Int32, :y => Int32)
    e.x, e.y = transformview(window, window.scene_stack[end], e.x, e.y)
    return (:mousebutton_up, e)
end

# function handleevent(::Val{SDL.MOUSEBUTTONDOWN}, e::Event)
#     button = e.data[17]
#     x = bitcat(Int32, e.data[24:-1:21])
#     y = bitcat(Int32, e.data[28:-1:25])
#     buttondown(button, x, y)
# end

# function handleevent(::Val{SDL.MOUSEBUTTONUP}, e::Event)
#     button = e.data[17]
#     x = bitcat(Int32, e.data[20:23])
#     y = bitcat(Int32, e.data[24:27])
#     buttonup(button, x, y)
# end

# buttondown(button, x, y) = nothing
# buttonup(button, x, y) = nothing

end #module
