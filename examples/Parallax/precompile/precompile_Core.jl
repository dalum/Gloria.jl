function _precompile_()
    precompile(Tuple{typeof(Core.Compiler.getindex), Tuple{typeof(Base.has_offset_axes), Bool}, Int64})
    precompile(Tuple{typeof(Core.Compiler.getindex), Tuple{Int64, Float64}, Int64})
    precompile(Tuple{typeof(Core.Compiler.getindex), Tuple{typeof(Base.round), DataType}, Int64})
    precompile(Tuple{typeof(Core.Compiler.getindex), Tuple{typeof(Base.schedule)}, Int64})
    precompile(Tuple{typeof(Core.Compiler.getindex), Tuple{typeof(Base.wait)}, Int64})
end
