(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     A = testinput(x-1,y)+testinput(x+1,y)
     in (map i=0,1 reduce(sum) A+[uint8](x) end)+A
end)
