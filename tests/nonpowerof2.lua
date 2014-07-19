(terralib.loadfile("test.t"))()
import "darkroom"

-- test that non-vector sized images work
local img = darkroomSimple.load("frame_126.bmp")

test(im(x,y) img(x,y)+[uint8](100) end)
