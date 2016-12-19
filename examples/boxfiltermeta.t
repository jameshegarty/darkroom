import "darkroom"
darkroomSimple = require("darkroomSimple")

a = darkroomSimple.load("frame10.bmp")
I = im(x,y) [int32](a) end

boxFilter = im(x,y) 0 end

for i=-5,5 do
  for j=-5,5 do
    boxFilter = im(x,y) boxFilter(x,y)+I(x+i,y+j) end
  end
end
boxFilterNorm = im(x,y) boxFilter/121 end

boxFilterUint8 = im(x,y) [uint8](boxFilterNorm) end
boxFilterUint8:save("out/boxfiltermeta.bmp")