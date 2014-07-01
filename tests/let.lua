terralib.require("test")
import "darkroom"

test(im(x,y)
      a = testinput(x,y)/2
      b = testinput(x,y)-100+a
      in [uint8](a+b)
     end)
