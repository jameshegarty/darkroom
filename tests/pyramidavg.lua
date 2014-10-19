(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x*2,y*2) end
im b(x,y) a(x*2,y*2) end
im c(x,y) b(x*2,y*2) end -- 8
im d(x,y) c(x*2,y*2) end -- 16

test(testinput(x,y)-d(x/16,y/16))
