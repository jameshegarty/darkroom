(terralib.loadfile("test.t"))()
import "darkroom"

local targetX = orionSimple.tap(int32, 1)
local targetY = orionSimple.tap(int32, 1)

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y) [uint8](
     darkroom.gather(testinput(x,y)+3,targetX,targetY,1,1,false)
                        )
end)