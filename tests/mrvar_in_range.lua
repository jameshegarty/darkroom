(terralib.loadfile("test.t"))()
import "darkroom"

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y)
     [uint8](
    map i=-1,1 j=-1,1 reduce(sum) 
      map ii=i,1 reduce(sum)
        testinput(x+ii,y+j)
      end
    end)
end)