package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require("test")
import "orion"


im c3(x,y) : cropShrink (testinput(x-20,y)/orion.uint8(2))+(testinput(x+20,y)/orion.uint8(2)) end

test(c3)
