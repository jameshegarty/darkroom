import "darkroom"
require "mapmachine"
require "image"

W = 128
H = 64
N = 3

-------------
s = d.leaf( terra( out : &uint8, a : &uint8[9])
              @out = (@a)[8]
            end, darkroom.type.uint(8), d.input( darkroom.type.array( darkroom.type.uint(8), {N*N} ) ) )
-------------
inp = d.input( darkroom.type.array( darkroom.type.uint(8), {W*H} ) )

convstencils = d.extractStencils( "convstencils", inp, W, -N+1, -N+1 )
convpipe = d.map( "conv", s, convstencils )

convpipe = d.fn( "convpipe", convpipe, inp )
-------------

convolve = d.compile( convpipe )

terra doit()
  var imIn : Image
  imIn:load("frame_128.bmp")
  var imOut : Image
  imOut:load("frame_128.bmp")

  convolve( [&uint8[W*H]](imOut.data), [&uint8[W*H]](imIn.data) )
  imOut:save("sttest.bmp")
end

doit()
