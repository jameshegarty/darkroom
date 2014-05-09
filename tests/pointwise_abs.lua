terralib.require "test"
import "darkroom"

im in1(x,y) orion.float32(testinput(x,y)) end
im in1(x,y) -in1(x,y) end
im out(x,y) orion.abs(in1(x,y)) end

test(out)