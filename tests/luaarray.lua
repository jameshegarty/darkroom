terralib.require("test")
import "darkroom"

local T = {1,2,1, --4
2,3,2, --7
4,5,6} --15

test(im(x,y) 
     [uint8](map i=-1,1 j=-1,1 reduce(sum) testinput(x,y)*T[j*3+i] end)
end)
