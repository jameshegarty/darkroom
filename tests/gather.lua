terralib.require "test"
import "darkroom"

local targetX = orionSimple.tap(orion.type.int(32))
local targetY = orionSimple.tap(orion.type.int(32))

darkroomSimple.setTap(targetX,1)
darkroomSimple.setTap(targetY,1)

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y) orion.uint8(
     orion.gather(testinput(x,y),targetX,targetY,1,1,false)
                        )
end)