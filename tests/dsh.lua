(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) {[uint8](x),[uint8](y),testinput(x*2-1,y*2+0)} end)
--test(im(x,y) {[uint8](x),[uint8](y),testinput} end)
