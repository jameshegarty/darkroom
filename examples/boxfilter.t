import "darkroom"
darkroomSimple = require("darkroomSimple")

a = darkroomSimple.load("frame10.bmp")
I = im(x,y) [int32](a) end

boxFilter = im(x,y)
  map i=-5,5 j=-5,5 reduce(sum) 
    I(x+i,y+j) 
  end
end
boxFilterNorm = im(x,y) boxFilter/121 end

boxFilterUint8 = im(x,y) [uint8](boxFilterNorm) end
boxFilterUint8:save("out/boxfilter.bmp")