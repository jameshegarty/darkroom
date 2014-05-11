terralib.require "test"
import "darkroom"

im a(x,y)  x+y*0 end
im b(x,y) 0*a(x-10,y+5)+1*a(x+10,y+7) end

test(im(x,y) orion.uint8( b(x-10,y)) end)
