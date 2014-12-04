import "darkroom"

-- This file demonstrates how to compile a darkroom pipeline
-- into a library file that can be linked with a c program.
-- See the associated clibrary.c

a = darkroom.input(uint8)
tap = darkroom.tap(uint8)

im pipeline1(x,y) a+[uint8](10) end
im pipeline2(x,y) a+tap end

compiledPipeline1 = darkroom.compile({a},{pipeline1},{},128,64)
compiledPipeline2 = darkroom.compile({a},{pipeline2},{tap},128,64)

terralib.saveobj("out/clibrary.o",{pipeline1=compiledPipeline1, pipeline2=compiledPipeline2})