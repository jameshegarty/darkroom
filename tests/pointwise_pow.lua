terralib.require("test")
import "darkroom"

test(im(x,y) [uint8](darkroom.pow(testinput(x,y)/10,2)) end)
