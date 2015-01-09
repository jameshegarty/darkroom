(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x-1,y-1)+testinput(x+1,y+1) end
im b(x,y) a(x-1,y-1)+a(x+1,y)+testinput end
im c(x,y) b(x-2,y-2)+b(x+2,y+2)+testinput end

test(c)
