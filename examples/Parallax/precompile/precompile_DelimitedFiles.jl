function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    precompile(Tuple{typeof(DelimitedFiles.writedlm), Base.IOContext{Base.GenericIOBuffer{Array{UInt8, 1}}}, Base.StackTraces.StackFrame, Char})
end
