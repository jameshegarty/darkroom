import "darkroom"
terralib.require("test")

width = 584
height = 388

--local frame1 = orion.image(orion.type.uint(8),width,height)
local frame1 = testinput

im vectorField(x,y) [uint8[3]](  {frame1(x,y),frame1(x,y)+3,0} ) end

test({vectorField, im(x,y) [uint8[3]]( {(vectorField(x,y)[0])*50+128, (vectorField(x,y)[1])*50+128,0}) end})