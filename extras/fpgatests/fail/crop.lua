(terralib.loadfile("test.t"))()
import "darkroom"

darkroomSimple.setImageSize(128,64)
im a(x,y) darkroom.crop([uint8](100)) end
test(im(x,y) a(x-2,y-2)+a(x+2,y+2) end)
