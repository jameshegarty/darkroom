import "darkroom"

darkroomSimple = terralib.require("darkroomSimple")

a = orionSimple.load("color.bmp")
im a(x,y) : float32[3] a(x,y) end

im blurx(x,y) : uint8[3] 
map i=-5,5 reduce(sum) a(x+i,y)/11 end
end

blurx:save("out/blurx.bmp")