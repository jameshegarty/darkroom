(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x,y) end
im b(x,y) testinput(x-1,y) end
im c(x,y) testinput(x,y-1) end
im d(x,y) testinput(x-1,y-1) end

test(im(x,y) (a(x*2,y*2)>>[uint8](2))+(b(x*2,y*2)>>[uint8](2))+(c(x*2,y*2)>>[uint8](2))+(d(x*2,y*2)>>[uint8](2)) end)
