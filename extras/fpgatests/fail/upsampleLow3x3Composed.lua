(terralib.loadfile("test.t"))()
import "darkroom"

local testinput = im(x,y) testinput(x*2,y*2) end
local testinput = im(x,y) testinput(x*2,y*2) end

im a(x,y) testinput(x/2,y/2) end
im b(x,y) (a(x-1,y)>>[uint8](2))+(a(x+1,y)>>[uint8](2))+(a(x,y-1)>>[uint8](2))+(a(x,y+1)>>[uint8](2)) end

im c(x,y) b(x/2,y/2) end
im d(x,y) (c(x-1,y)>>[uint8](2))+(c(x+1,y)>>[uint8](2))+(c(x,y-1)>>[uint8](2))+(c(x,y+1)>>[uint8](2)) end

test(d)
