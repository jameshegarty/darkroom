terralib.require "test"
import "darkroom"

im a(x,y)  x+y*2 end
im b(x,y)  a(x-10,y+5) end

test(im(x,y) orion.uint8(b(x-10,y)) end)
