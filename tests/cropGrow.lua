package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require("test")
import "orion"

im c1(x,y) : crop(0,0,100,100) testinput(x,y) end
im c2(x,y) : crop(10,10,100,100) testinput(x,y) end

im c3(x,y) : cropGrow (c1(x,y)/orion.uint8(2))+(c2(x,y)/orion.uint8(2)) end

test(c3)
