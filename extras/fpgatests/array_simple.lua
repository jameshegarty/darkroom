(terralib.loadfile("test.t"))()
import "darkroom"

darkroomSimple.setImageSize(128,64)

im a(x,y) {1,2,3} end
im b(x,y) {2,4,6} end

im final(x,y) [uint8[3]]( (a(x,y)+b(x,y))*10 ) end

test({{final,"uart", darkroom.type.array(darkroom.type.uint(8),3)}})

-- should print {30,60,90}