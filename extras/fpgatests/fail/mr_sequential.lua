(terralib.loadfile("test.t"))()
import "darkroom"

-- First a should be computed then be. This should be 9+9 iterations, not 9*9
im a(x,y) [uint8](map ii=-2,2 jj=-2,2 reduce(sum)  testinput(x+ii,y+jj) >> [uint8](2) end) end
test(im b(x,y) [uint8](map i=-1,1 j=-1,1 reduce(sum) (testinput(x+i,y+j) >> [uint8](2))+ (a>> [uint8](3)) end) end)