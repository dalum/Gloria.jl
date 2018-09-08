mutable struct Audio <: AbstractResource
    ptr::Ptr{SDL.Mix_Chunk}
    filename::String
    channels::Set{Int32}

    function Audio(ptr::Ptr{SDL.Mix_Chunk}, filename::String)
        self = new(ptr, filename, Set{Int32}())
        finalizer(destroy!, self)
        return self
    end
end

function Audio(resources::Resources, filename::String)
    if filename in keys(resources)
        @debug("resource already loaded: '$filename'")
        return resources[filename]::Audio
    end

    self = Audio(load(query(filename)), filename)
    resources[filename] = self
    return self
end

function destroy!(a::Audio)
    SDL.Mix_FreeChunk(a.ptr)
    a.ptr = C_NULL
    return nothing
end

load(f::File{format"WAV"}) = Gloria.SDL.Mix_LoadWAV(f.filename)
load(f::File{format"OGG"}) = Gloria.SDL.Mix_LoadWAV(f.filename)
load(f::File{format"MP3"}) = Gloria.SDL.Mix_LoadWAV(f.filename)

function play!(audio::Audio; repeat::Int = 0, channel::Int = -1, volume::Int = 100)
    channel = SDL.Mix_PlayChannel(convert(Int32, channel)::Int32, audio.ptr, convert(Int32, repeat)::Int32)
    SDL.Mix_Volume(channel, convert(Int32, volume)::Int32)
    push!(audio.channels, channel)
    return audio
end

function stop!(audio::Audio)
    for channel in audio.channels
        # Only halt channels where the last loaded chunk was `audio`
        SDL.Mix_GetChunk(channel) == audio.ptr && SDL.Mix_HaltChannel(channel)
    end
    empty!(audio.channels)
    return audio
end

allocate_audiochannels!(n::Int) = SDL.Mix_AllocateChannels(convert(Int32, n))
