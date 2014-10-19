(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x*2,y*2) end  -- 1/2
im b(x,y) a(x/7,y/7) end -- 7/2
im c(x,y) a(x/3,y/3) end -- 3/2

test({b,c})
