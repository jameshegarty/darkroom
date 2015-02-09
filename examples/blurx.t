import "darkroom"
require("darkroomDebug")

darkroomSimple = require("darkroomSimple")

a = darkroomSimple.load("color.bmp")
im a(x,y) [float[3]]( a(x,y) ) end

im blurx(x,y) [uint8[3]](
  map i=-5,5 reduce(sum) a(x+i,y)/11 end)
end

blurx:save("out/blurx.bmp")