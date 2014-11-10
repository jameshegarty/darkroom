(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x*3,y*3) end  -- 1/3
im b(x,y) a(x/7,y/7) end -- 7/3

test(b)
