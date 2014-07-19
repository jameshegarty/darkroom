(terralib.loadfile("test.t"))()
import "darkroom"

-- test that non-vector sized images work
local img = darkroomSimple.load("frame_126.bmp")

im a(x,y) [uint8]( map i=-1,1 reduce(sum) img(x+i,y)/3 end) end
im a(x,y) [uint8]( map i=-1,1 reduce(sum) a(x+i,y)/3 end) end
im a(x,y) [uint8]( map i=-1,1 reduce(sum) a(x+i,y)/3 end) end

test(a)
