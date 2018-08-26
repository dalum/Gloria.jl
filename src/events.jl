struct Event
    fields::Dict{Symbol, Any}
end
Event() = Event(Dict{Symbol, Any}())
Event(pairs::Pair...) = Event(Dict{Symbol, Any}(pairs...))

Base.getproperty(e::Event, name::Symbol) = getfield(e, :fields)[name]
Base.setproperty!(e::Event, name::Symbol, x) = setindex!(getfield(e, :fields), x, name)

function bitcat(::Type{T}, arr)::T where T<:Number
    out = zero(T)
    for x in arr
        out <<= sizeof(x)*8
        out |= convert(T, x)
    end
    out
end

function geteventdata(data::Vector{UInt8}, pairs::Pair{Symbol,DataType}...)
    e = Event()
    i = 1
    for pair in pairs
        s = sizeof(pair.second)
        setindex!(getfield(e, :fields), bitcat(pair.second, data[i+s-1:-1:i]), pair.first)
        i += s
    end
    return e
end

eventtype(data::Vector{UInt8}) = bitcat(UInt32, data[4:-1:1])

parseevent(window::Window, data::Vector{UInt8}) = parseevent(window, Val(eventtype(data)), data)
parseevent(window::Window, ::Val, data::Vector{UInt8}) = (:notsupported, Event())

function parseevent(window::Window, ::Val{SDL.QUIT}, data::Vector{UInt8})
    e = geteventdata(data, :type => UInt32, :timestamp => UInt32)
    return (:quit, e)
end
