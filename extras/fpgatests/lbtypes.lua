(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) [uint16](testinput(x,y)) << [uint16](2) end
im b(x,y) a(x-1,y+1)+a(x,y+1)+a(x+1,y+1)+a(x-1,y)+a(x,y)+a(x+1,y)+a(x-1,y-1)+a(x,y-1)+a(x+1,y-1) end
im c(x,y) [int32](b) end
im d(x,y) c(x-1,y+1)+c(x,y+1)+c(x+1,y+1)+c(x-1,y)+c(x,y)+c(x+1,y)+c(x-1,y-1)+c(x,y-1)+c(x+1,y-1) end

test(im(x,y) [uint8](d>>[int32](7)) end)

