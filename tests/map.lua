(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) map i=0,2 reduce(none) testinput+[uint8](i*20) end end)