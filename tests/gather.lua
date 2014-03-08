package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require "test"
import "orion"

local targetX = orion.tap(orion.type.int(32), "targetX")
local targetY = orion.tap(orion.type.int(32), "targetY")

orion.setTap(targetX,1)
orion.setTap(targetY,1)

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y) : uint8
     orion.gather(testinput(x,y),targetX,targetY,1,1,false)
end)