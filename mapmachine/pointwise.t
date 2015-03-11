import "darkroom"
require "mapmachine"
require "image"
local ffi = require("ffi")

W = 128
H = 64

plus100 = d.leaf( terra( out : &uint8, a : &uint8 ) @out =  @a+100 end, darkroom.type.uint(8), d.input( darkroom.type.uint(8) ) )

inp = d.input( darkroom.type.array( darkroom.type.uint(8), {W*H} ) )
out = d.map( "plus100",plus100, inp )
fn = d.fn( out, inp )

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

--  for i=0,W*H-1 do  out[i] = inp[i]+100 end
--  return out
--  return ffi.new("unsigned char[8192]",out)
--  return ffi.new("unsigned char*",out)
--end

local res = d.compile( fn )
res:printpretty(false)
--save(res(load("frame_128.bmp")), "out.bmp")

terra doit()
  var imIn : Image
  imIn:load("frame_128.bmp")
  var imOut : Image
  imOut:load("frame_128.bmp")

  res( [&uint8[W*H]](imOut.data), [&uint8[W*H]](imIn.data) )
  imOut:save("pointwise.bmp")
end

doit()