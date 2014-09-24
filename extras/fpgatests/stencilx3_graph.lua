(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x,y)+[uint8](100) end
im b(x,y) (a(x-2,y)+a(x-1,y)+a(x,y)+a(x+1,y)) >> [uint8](2) end
im c(x,y) (a(x+4,y)+a(x+5,y)+a(x+6,y)) >> [uint8](2) end
im d(x,y) (b(x-8,y)+b(x-9,y)+b(x-10,y)+c(x+5,y)+c(x+6,y)+c(x+7,y)) >> [uint8](2) end

test(d)
