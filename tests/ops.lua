(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) [uint8](darkroom.sqrt(darkroom.arctan([float](testinput(x,y))))*[float](255)) end)
