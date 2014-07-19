(terralib.loadfile("test.t"))()
import "darkroom"

-- test that chains of translates work
im a(x,y) testinput(x-1,y) + testinput(x+1,y) end
im b(x,y) a(x+1,y) end
im c(x,y) b(x+1,y) end
im d(x,y) c(x+1,y) end

test(d)
