(terralib.loadfile("test.t"))()
import "darkroom"

color = darkroomSimple.load("color.bmp")
test(im(x,y) [uint8](map i=0,2 reduce(sum) color[i]/3 end) end)