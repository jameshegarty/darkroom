(terralib.loadfile("test.t"))()
import "darkroom"

--im a(x,y) testinput(x,y) >> [uint8](3) end
--test(im(x,y) a(x-1,y+1)+a(x,y+1)+a(x+1,y+1)+a(x-1,y)+a(x,y)+a(x+1,y)+a(x-1,y-1)+a(x,y-1)+a(x+1,y-1) end)

im a(x,y) testinput(x,y) >> [uint8](2) end
test(im(x,y) a(x,y)+a(x,y-1)+a(x,y-2)+a(x,y-3) end)

