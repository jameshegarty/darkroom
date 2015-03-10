import "darkroom"
require "mapmachine"
require "image"

searchRadius = 60
windowRadius = 4

SADSize = (windowRadius*2+1)*(windowRadius*2+1)
W = 720
H = 405

--------------------------------
-- AD (int8,int8)->int32
A = d.input( darkroom.type.uint(8) )
B = d.input( darkroom.type.uint(8) )

AD = d.leaf( terra(A:uint8, B:uint8) cstdlib.abs([int32](A)-[uint32](B)) end, darkroom.type.int(32), A, B )

--------------------------------
-- SAD (uint8[SADSize], uint8[SADSize]) -> int32
A = d.input( darkroom.type.array( darkroom.type.uint(8),{SADSize}) )
B = d.input( darkroom.type.array( darkroom.type.uint(8),{SADSize}) )

ADMap = d.map( AD, A, B )
SAD = d.reduce( "sum", ADMap ) -- (int32[SADSize] -> uint32)
SAD = d.fn( SAD, A, B )

--------------------------------
-- stereoKernel ( (uint8[SADSize])[searchRadius], (uint8[SADSize])[searchRadius] ) -> uint8 )
frame1stencils = d.input( darkroom.type.array( darkroom.type.uint(8), {SADSize, searchRadius}) )
frame2stencils = d.input( darkroom.type.array( darkroom.type.uint(8), {SADSize, searchRadius}) )

SADarray = d.map( SAD, frame1stencils, frame2stencils ) -- uint8[searchRadius)

match = d.reduce( "argmin", SADarray ) -- uint8

stereoKernel = d.fn( match, frame1stencils, frame2stencils )

-------------------------------
dupInput = d.input( darkroom.type.array( darkroom.type.uint(8), {SADSize} ) )
dupStencil = d.dup( dupInput, searchRadius )
dupStencil = d.fn( dupStencil, dupInput )
-------------------------------
-- stereo (uint8[W*H], uint8[W*H]) -> uint8[W*H]

frame1 = d.input( darkroom.type.array( darkroom.type.uint(8), {W*H}) )
frame2 = d.input( darkroom.type.array( darkroom.type.uint(8), {W*H}) )

frame1st = d.extractStencils( frame1, W, -windowRadius*2, -windowRadius*2 ) -- (uint8[SADSize])[W*H]
frame1st = d.map( dupStencil, frame1st )
-- extractStencilArray is a function of extractStencils, slice
frame2st = d.extractStencilArray( frame2, W, -windowRadius*2, -windowRadius*2, searchRadius ) -- ((uint8[windowRadius*2+1][windowRadius*2+1])[searchRadius])[W*H]

print("SR",frame1st.type, frame2st.type)
stereoResult = d.map( stereoKernel, frame1st, frame2st )
stereo = d.fn( stereoResult, frame1, frame2 )

---------------------
-- apply
stereo = d.compile( stereo )

terra load( filename : &int8)
  var img : Image
  img:load( filename )
  return [&uint8](img.data)
end


terra save( d : &uint8, filename : &int8)
  var img : Image
  img:initSimple(128,64,1,8,false,false,false,false,d)
  img:save(filename)
end

save( stereo( load("left0224.bmp"),load("right0224.bmp")), "stereo.bmp")
