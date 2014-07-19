(terralib.loadfile("test.t"))()
import "darkroom"

local targetX = darkroomSimple.tap( int32, 5 )
local targetY = darkroomSimple.tap( int32, 1 )

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y)
     [uint8](
    map i=-10,10 j=-1,1 reduce(sum) 
      testinput(x+i,y+j)*(if i==targetX and j==targetY then 1 else 0 end) 
    end)
end)