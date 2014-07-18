terralib.require "test"
import "darkroom"

im in1(x,y) [float](testinput(x,y)) end
im in1(x,y) -in1(x,y) end
im out(x,y) [uint8](darkroom.abs(in1(x,y))) end

test(out)