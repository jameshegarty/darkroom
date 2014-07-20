(terralib.loadfile("test.t"))()
import "darkroom"

im inp(x,y)  [int32](testinput(x,y)) end

im A(x,y) inp(x,y)+[uint8](10) end
im B(x,y) [uint8]((inp(x,y)+A(x-10,y))/2) end
im C(x,y) [uint8]((A(x,y)+A(x,y))/2) end

test({B,C})
