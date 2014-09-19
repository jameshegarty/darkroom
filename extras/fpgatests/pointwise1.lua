(terralib.loadfile("test.t"))()
import "darkroom"

im out1(x,y)  testinput(x,y) >> [uint8](1) end
im out2(x,y) testinput(x,y) >> [uint8](1) end
im out(x,y)  out1(x,y) + out2(x,y) + [uint8](100) end

test(out)