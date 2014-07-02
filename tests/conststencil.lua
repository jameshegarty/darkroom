terralib.require "test"
import "darkroom"

darkroomSimple.setImageSize(128,64)

im a(x,y) 30 end

test(im(x,y) [uint8](a(x-4,y-4) + a(x-3,y-3)) end)