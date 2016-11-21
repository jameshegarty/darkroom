import "darkroom"
darkroomSimple = require("darkroomSimple")

a = darkroomSimple.load("frame10.bmp")
im a(x,y) [uint32]( a(x,y) ) end

im convolved(x,y) map i=-2,2 j=-2,2 reduce(sum) a(x+i,y+j) end end
im convolvedUint8(x,y) [uint8](convolved(x,y)/25) end

convolvedUint8:save("out/convolve.bmp")