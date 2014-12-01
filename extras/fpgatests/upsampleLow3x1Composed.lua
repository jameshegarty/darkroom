(terralib.loadfile("test.t"))()
import "darkroom"

local testinput = im(x,y) testinput(x*2,y) end
local testinput = im(x,y) testinput(x*2,y) end

im a(x,y) testinput(x/2,y) end
im b(x,y) (a(x-1,y)>>[uint8](1))+(a(x+1,y)>>[uint8](1)) end

im c(x,y) b(x/2,y) end
im d(x,y) (c(x-1,y)>>[uint8](1))+(c(x+1,y)>>[uint8](1)) end

test(d)
