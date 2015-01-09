(terralib.loadfile("test.t"))()
import "darkroom"


test(im(x,y) [uint8]((x+y)+(testinput(x-1,y)+testinput(x+1,y)+testinput(x,y-1)+testinput(x,y+1))) end)
