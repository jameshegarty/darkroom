package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require("test")
import "orion"

local square = orion.tapLUT(orion.type.uint(8),256,"square")
local st = {}
for i=0,255 do table.insert(st, i*i/(255)) end
--for i=0,255 do table.insert(st, math.sin(i/255)*255) end
--for i=0,255 do if i < 100 then table.insert(st, 0) else table.insert(st,i) end end
orion.setTapLUT(square,st)

test(im(x,y) square[testinput(x,y)] end)
