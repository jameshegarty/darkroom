ffi = require "ffi"

terralib.require("test")
terralib.require("common")
import "darkroom"

maxVelocity = 1

print("WH",testinput:width(),testinput:height())

u_data = ffi.new("float[?]",testinput:width()*testinput:height())
v_data = ffi.new("float[?]",testinput:width()*testinput:height())

math.randomseed(1234)
for j=0,testinput:height()-1 do
  for i=0,testinput:width()-1 do
    u_data[j*testinput:width()+i] = math.random()*2-1
    v_data[j*testinput:width()+i] = math.random()*2-1
  end
end

local terra makeIm(data : &float)
  var img : Image
  cstdio.printf("make Im %d %d\n",[testinput:width()],[testinput:height()])
  img:initSimple([testinput:width()],[testinput:height()],1,32,true,false,true,data)
  return img
end

u_im = makeIm(u_data)
local u = darkroomSimple.image(orion.type.float(32),testinput:width(),testinput:height())
orion.bindImage( u:id(), u_im )

v_im = makeIm(v_data)
local v = darkroomSimple.image(orion.type.float(32),testinput:width(),testinput:height())
orion.bindImage( v:id(), v_im )


test({im(x,y) [resampleBilinear(
    false,
    testinput, 
    maxVelocity, 
    maxVelocity, 
    u,v)] end,
im(x,y) [resampleBilinearSlow(
    false,
    testinput, 
    maxVelocity, 
    maxVelocity, 
    u,v)] end})

