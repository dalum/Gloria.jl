module Mouse

import Gloria: handleevent
using Gloria: SDL, Event, bitcat

function getmousestate()
    x,y = Int[1], Int[1]
    SDL.GetMouseState(pointer(x), pointer(y))
    return (x=x[], y=y[])
end

function handleevent(::Val{SDL.MOUSEBUTTONDOWN}, e::Event)
    button = e.data[17]
    x = bitcat(Int32, e.data[24:-1:21])
    y = bitcat(Int32, e.data[28:-1:25])
    buttondown(button, x, y)
end

function handleevent(::Val{SDL.MOUSEBUTTONUP}, e::Event)
    button = e.data[17]
    x = bitcat(Int32, e.data[20:23])
    y = bitcat(Int32, e.data[24:27])
    buttonup(button, x, y)
end

buttondown(button, x, y) = nothing
buttonup(button, x, y) = nothing

end #module
