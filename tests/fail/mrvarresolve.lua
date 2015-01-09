(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) map i=-1,1 j=-1,1 reduce(sum) 
     i=0
     j=0
     in testinput(x+i,y+j)/[uint8](9) end end)
