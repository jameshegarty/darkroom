(terralib.loadfile("test.t"))()
import "darkroom"

local testinput = im(x,y) testinput(x*2,y*2) end
local testinput = im(x,y) testinput(x*2,y*2) end

im a(x,y) (testinput(x/2-1,y/2)>>[uint8](2))+(testinput(x/2+1,y/2)>>[uint8](2))+(testinput(x/2,y/2-1)>>[uint8](2))+(testinput(x/2,y/2+1)>>[uint8](2)) end
im b(x,y) (a(x/2-1,y/2)>>[uint8](2))+(a(x/2+1,y/2)>>[uint8](2))+(a(x/2,y/2-1)>>[uint8](2))+(a(x/2,y/2+1)>>[uint8](2)) end

test(b)
