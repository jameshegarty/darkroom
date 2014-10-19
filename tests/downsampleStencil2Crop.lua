(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) darkroom.crop(testinput(x*2,y*2)) end
test(im(x,y) (a(x,y)/[uint8](4))+(a(x-1,y)/[uint8](4))+(a(x,y-1)/[uint8](4))+(a(x-1,y-1)/[uint8](4)) end)
