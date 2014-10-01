(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y)
     [uint8](
    map i=-1,1 reduce(sum) 
    testinput(x+i,y) >> [uint8](2)
    end)
end)