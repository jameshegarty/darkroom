terralib.require "test"
import "darkroom"

darkroomSimple.setImageSize(128,64)

im a(x,y)  x+y*0 end
im b(x,y) 0*a(x-10,y+5)+1*a(x+10,y+7) end

test(im(x,y) [uint8]( b(x-10,y)) end)
