(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) (testinput(x/2-1,y/2)/[uint8](4))+(testinput(x/2+1,y/2)/[uint8](4))+(testinput(x/2,y/2-1)/[uint8](4))+(testinput(x/2,y/2+1)/[uint8](4)) end
im b(x,y) (a(x/2-1,y/2)/[uint8](4))+(a(x/2+1,y/2)/[uint8](4))+(a(x/2,y/2-1)/[uint8](4))+(a(x/2,y/2+1)/[uint8](4)) end

test(b)
