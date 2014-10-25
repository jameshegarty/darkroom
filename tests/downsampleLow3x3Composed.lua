(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x*2,y*2) end
im b(x,y) (a(x-1,y)/[uint8](4))+(a(x+1,y)/[uint8](4))+(a(x,y-1)/[uint8](4))+(a(x,y+1)/[uint8](4)) end

im c(x,y) b(x*2,y*2) end
im d(x,y) (c(x-1,y)/[uint8](4))+(c(x+1,y)/[uint8](4))+(c(x,y-1)/[uint8](4))+(c(x,y+1)/[uint8](4)) end

test(d)
