(terralib.loadfile("test.t"))()
import "darkroom"

I = darkroomSimple.load("color.bmp")

test(im(x,y) darkroom.filter(x%32==0 and y%32==0,I) end)
