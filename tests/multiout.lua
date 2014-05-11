terralib.require("test")
import "darkroom"

im inp(x,y)  orion.int32(testinput(x,y)) end

im A(x,y) inp(x,y)+orion.uint8(10) end
im B(x,y) orion.uint8((inp(x,y)+A(x-10,y))/2) end
im C(x,y) orion.uint8((A(x,y)+B(x,y))/2) end

test({B,C})
