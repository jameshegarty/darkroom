package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require "test"
import "orion"

local targetX = orion.tap(orion.type.int(32), "targetX")
local targetY = orion.tap(orion.type.int(32), "targetY")

orion.setTap(targetX,1)
orion.setTap(targetY,1)

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y) : uint8
  map i=-1,1 j=-1,1 reduce(sum) 
    testinput(x+i,y+j)*(if i==targetX and j==targetY then 1 else 0 end) 
  end
end)