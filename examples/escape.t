import "darkroom"
darkroomSimple = require("darkroomSimple")

a = darkroomSimple.load("frame10.bmp")
I = im(x,y) [float](a) end

N = 0.1 + math.sin(15)
mult = im(x,y) I(x,y) * [N] end

multUint8 = im(x,y) [uint8](mult) end
multUint8:save("out/escape.bmp")