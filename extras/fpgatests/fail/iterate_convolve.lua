(terralib.loadfile("test.t"))()
import "darkroom"

-- sums to 256
local K={1,4,7,4,1,
                         4,15,24,15,4,
                         7,24,36,24,7,
                         4,15,24,15,4,
                         1,4,7,4,1} 

test(im(x,y) 
     iterate i=0,4 A=darkroom.gatherColumn(testinput,i-2,1,-2,2,-2,2) reduce(sum)
       map j=-2,2 reduce(sum) [uint8](([uint16](A[j+2])*[uint16](K[(j+2)*5 + i]))>>[uint16](8)) end
     end
end)
