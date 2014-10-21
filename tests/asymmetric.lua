(terralib.loadfile("test.t"))()
import "darkroom"
--terralib.require("darkroomDebug")
--darkroomSimple = terralib.require("darkroomSimple")

I = darkroomSimple.load("frame10.bmp")
--I = darkroomSimple.load("color.bmp")
--I = testinput
I = im(x,y) [float[3]](I) end

bx = im(x,y) (I(x-1,y) + I(x,y) + I(x+1,y))/3 end
by = im(x,y) (bx(x,y-1) + bx(x,y) + bx(x,y+1))/3 end
--difference = im(x,y) I(x,y)-by(x,y) end
--scaled = im(x,y) 0.8 * difference(x,y) end
--sharpened = im(x,y) I(x,y) + scaled(x,y) end

-- clamp
--R = im(x,y) darkroom.vectorSelect(sharpened>255, [float[3]](255), sharpened) end
--R = im(x,y) darkroom.vectorSelect(R<0, [uint8[3]](0), [uint8[3]](R)) end
by = im(x,y) [uint8[3]](by) end
--by:save("out/unsharpmask.bmp",{debug=true,verbose=true})
test(by)