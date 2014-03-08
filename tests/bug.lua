package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require("test")
import "orion"

im valueXY(x,y) : cropNone orion.float32(testinput(x-1,y-1)) end

test(im(x,y) :cropSame valueXY(x,y) end)
