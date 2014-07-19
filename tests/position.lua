(terralib.loadfile("test.t"))()
import "darkroom"

darkroomSimple.setImageSize(128,64)

im a(x,y)  x+y*2 end
im b(x,y)  a(x-10,y+5) end

test(im(x,y) [uint8](b(x-10,y)) end)
