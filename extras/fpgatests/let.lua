(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y)
      a = testinput(x,y) >> [uint8](1)
      b = testinput(x,y)-[uint8](100)+a
      in [uint8](a+b)
     end)
