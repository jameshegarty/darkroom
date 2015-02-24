(terralib.loadfile("test.t"))()
import "darkroom"

local targetX = 5
local targetY = 1

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y)
     [uint8](map i = 0,2 reduce(sum)
      map ii=-1,1 jj=-1,1 reduce(sum) -- SAD
      testinput(x+i+ii,y+jj)+[uint8](3)
      end
      end)
end)