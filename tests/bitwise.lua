(terralib.loadfile("test.t"))()
import "darkroom"

darkroomSimple.setImageSize(128,64)

-- should be color 154
test(im(x,y) not (([uint8](100) or [uint8](1)) and [uint8](101)) end)
