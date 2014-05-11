terralib.require("test")
import "darkroom"

im a(x,y) {1,2,3} end
im b(x,y) {2,4,6} end

test(im(x,y) orion.array3uint8( (a(x,y)+b(x,y))*10 ) end)

-- should print {30,60,90}