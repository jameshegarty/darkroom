terralib.require "test"
import "darkroom"

-- a quantization filter

im in1(x,y) orion.float32(testinput(x,y)) end
im out(x,y) orion.floor(in1(x,y)/orion.uint8(50))*orion.uint8(50) end

test(out)

