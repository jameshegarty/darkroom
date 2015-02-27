(terralib.loadfile("test.t"))()
import "darkroom"

local targetX = im(x,y) [int8](1) end
local targetY = im(x,y) [int8](-1) end

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y) [uint8](darkroom.gather(testinput(x,y)+[uint8](3),targetX,targetY,1,1)) end)