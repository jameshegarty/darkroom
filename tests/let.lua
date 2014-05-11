terralib.require("test")
import "darkroom"

test(im(x,y) orion.uint8(
     let
      a = testinput(x,y)/2
      b = testinput(x,y)-100+a
      in a+b)
     end)
