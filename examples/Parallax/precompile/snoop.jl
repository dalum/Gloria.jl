using SnoopCompile

### Log the compiles
# This only needs to be run once (to generate "/tmp/images_compiles.csv")
SnoopCompile.@snoop1 abspath(@__DIR__, "compiles.csv") begin
    include(abspath(@__DIR__, "..", "parallax.jl"))
end

### Parse the compiles and generate precompilation scripts
# This can be run repeatedly to tweak the scripts
blacklist = []

data = SnoopCompile.read(abspath(@__DIR__, "compiles.csv"))
pc = SnoopCompile.parcel(reverse!(data[2]), blacklist=blacklist)
SnoopCompile.write(abspath(@__DIR__), pc, always=true)
