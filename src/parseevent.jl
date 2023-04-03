function geteventdata(TYPE::Symbol, data::Base.RefValue{SDL.SDL_Event}, pairs::Pair{Symbol,DataType}...)
    e = Event{TYPE}()
    for pair in pairs
        setindex!(getfield(e, :fields), 
        	getproperty(getproperty(data[],TYPE),pair.first), pair.first)
    end
    return e
end

function geteventdata(TYPE::Symbol, data::SDL.SDL_KeyboardEvent, symbols::Pair{Symbol,DataType}...)
    e = Event{TYPE}()
    # get(pairs, pair.first, pair.first)
    for pair in first(symbols,5)
        setindex!(getfield(e, :fields), 
        	getproperty(data,pair.first), pair.first)
    end
    for pair in last(symbols,4)
        setindex!(getfield(e, :fields), 
        	getproperty(getproperty(data,:keysym),
            pair.first)|>x->x isa Real ? x : pair.second(x), pair.first)
    end
    return e
end
function geteventdata(TYPE::Symbol, data, symbols::Pair{Symbol,DataType}...)
    e = Event{TYPE}()
    for pair in symbols
        setindex!(getfield(e, :fields), 
        	getproperty(data,pair.first), pair.first)
    end
    return e
end

eventtype(event::Base.RefValue{SDL.SDL_Event}) = event[].type

parseevent(window::Window, event::Base.RefValue{SDL.SDL_Event}) = parseevent(window, Val(eventtype(event)), event)
parseevent(window::Window, ::Val, event::Base.RefValue{SDL.SDL_Event}) = Event{:notsupported}()
function parseevent(window::Window, ::Val{UInt32(SDL.SDL_QUIT)}, event::Base.RefValue{SDL.SDL_Event})
    return geteventdata(:quit, event, :type => UInt32, :timestamp => UInt32)
end
