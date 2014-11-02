(terralib.loadfile("test.t"))()
import "darkroom"

local targetX = 5
local targetY = 1

K = {2,0,0,0,4,0,0,0,2}
-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y)
     [uint8](
    map i=-1,1 j=-1,1 reduce(sum) 
    testinput(x+i,y+j)*K[(i+1)*3+j+1]
    end)
end)