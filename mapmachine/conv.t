import "darkroom"
require "mapmachine"
require "image"

W = 128
H = 64
N = 3

-------------
partial = d.leaf( terra( a : uint8, i : uint8)
                    return [int32](a)*[int32](i)
                  end, darkroom.type.int(32), d.input( darkroom.type.uint(8) ), d.input( darkroom.type.uint(8) ) )
-------------
touint8 = d.leaf( terra( a : int32 ) return [uint8](a / 45) end, darkroom.type.uint(8), d.input( darkroom.type.int(32) ) )
-------------
inp = d.input( darkroom.type.array( darkroom.type.uint(8), {N*N} ) )
r = d.range( N*N, darkroom.type.uint(8) )
conv = d.map( partial, inp, r )
conv = d.reduce( "sum", conv )
conv = d.apply( touint8, conv )

convolve = d.fn( conv, inp )
-------------
inp = d.input( darkroom.type.array( darkroom.type.uint(8), {W*H} ) )

convstencils = d.extractStencils( inp, W, -N+1, -N+1 )
convpipe = d.map( convolve, convstencils )

convpipe = d.fn( convpipe, inp )
-------------

convolve = d.compile( convpipe )

--[=[
terra doit()
  var img : Image
  img:load( "frame128.bmp" )
  var out = convolvefn(img.data)
  img.data = out
  img:save( "out.bmp" )
end

doit()

]=]

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

save( convolve( load("frame_128.bmp")), "conv.bmp")
--local l = load("frame_128.bmp")

--print(l,l[0],l[1],l[2],l[3])