package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require "test"
import "orion"

-- a quantization filter

im in1(x,y) : float32  testinput(x,y) end
im out(x,y)  orion.floor(in1(x,y)/orion.uint8(50))*orion.uint8(50) end


test(out)

