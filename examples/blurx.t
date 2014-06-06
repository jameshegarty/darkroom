import "darkroom"
terralib.require("darkroomDebug")

darkroomSimple = terralib.require("darkroomSimple")

a = darkroomSimple.load("color.bmp")
im a(x,y) orion.array3float32( a(x,y) ) end

im blurx(x,y) orion.array3uint8(
  map i=-5,5 reduce(sum) a(x+i,y)/11 end)
end

blurx:save("out/blurx.bmp")