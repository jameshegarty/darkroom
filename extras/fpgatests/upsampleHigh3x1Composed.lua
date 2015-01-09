(terralib.loadfile("test.t"))()
import "darkroom"

local testinput = im(x,y) testinput(x*2,y) end
local testinput = im(x,y) testinput(x*2,y) end

im a(x,y) (testinput(x/2-1,y)>>[uint8](1))+(testinput(x/2+1,y)>>[uint8](1)) end
im b(x,y) (a(x/2-1,y)>>[uint8](1))+(a(x/2+1,y)>>[uint8](1)) end

test(b)
