(terralib.loadfile("test.t"))()
import "darkroom"

darkroomSimple.setImageSize(128,64)

test(im(x,y) 
     iterate i=0,1 reduce(sum)
     [uint8](5+i*10)
     end
end)
