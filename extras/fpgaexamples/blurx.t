import "darkroom"
fpgaEstimate = terralib.require("fpgaEstimate")

darkroomSimple = terralib.require("darkroomSimple")

a = darkroomSimple.load("../../examples/color.bmp")
im a(x,y) [uint16[3]]( a(x,y) ) end

im blurx(x,y) [uint8[3]](
  map i=-8,7 reduce(sum) a(x+i,y) >> 4 end)
end

blurx:save("out/blurx.bmp")

local est = fpgaEstimate.compile({blurx}, 1920)
io.output("out/blurxEstimate.txt")
io.write(est)
io.close()