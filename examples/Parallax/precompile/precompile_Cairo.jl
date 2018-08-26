function _precompile_()
    precompile(Tuple{typeof(Cairo.destroy), Cairo.CairoContext})
    precompile(Tuple{getfield(Cairo, Symbol("##CairoImageSurface#1")), Bool, typeof(identity), Array{UInt32, 2}, Int32})
    precompile(Tuple{typeof(Cairo.destroy), Cairo.CairoSurface{UInt32}})
end
