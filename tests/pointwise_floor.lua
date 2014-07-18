terralib.require "test"
import "darkroom"

-- a quantization filter

im in1(x,y) [float](testinput(x,y)) end
im out(x,y) [uint8](darkroom.floor(in1(x,y)/[uint8](50)))*[uint8](50) end

test(out)

