(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) [uint8]([int16](testinput(x,y))*[int16](-3))+[uint8]([int16]([int8](-32))>>[uint8](1)) end)
