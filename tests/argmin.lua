(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y)
  [uint8[3]](map i=-1,1 j=-1,1 reduce(argmin) testinput(x+i,y+j) end)
end)