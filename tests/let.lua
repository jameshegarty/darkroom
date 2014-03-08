package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require("test")
import "orion"

test(im(x,y) : uint8
     let
      a = testinput(x,y)/2
      b = testinput(x,y)-100+a
     in a+b
     end)
