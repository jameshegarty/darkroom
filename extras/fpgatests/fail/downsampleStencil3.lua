(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) (testinput(x*2,y*2)>>[uint8](2))+(testinput(x*2-1,y*2)>>[uint8](2))+(testinput(x*2,y*2-1)>>[uint8](2))+(testinput(x*2-1,y*2-1)>>[uint8](2)) end)
