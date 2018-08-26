function _precompile_()
    precompile(Tuple{typeof(LinearAlgebra.BLAS.gemv!), Char, Float64, Array{Float64, 2}, Array{Float64, 1}, Float64, Array{Float64, 1}})
    precompile(Tuple{typeof(LinearAlgebra.generic_matvecmul!), Array{Float64, 1}, Char, Array{Float64, 2}, Array{Float64, 1}})
    precompile(Tuple{typeof(LinearAlgebra.gemv!), Array{Float64, 1}, Char, Array{Float64, 2}, Array{Float64, 1}})
end
