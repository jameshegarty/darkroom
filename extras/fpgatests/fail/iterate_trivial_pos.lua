(terralib.loadfile("test.t"))()
import "darkroom"

darkroomSimple.setImageSize(128,64)

test(im(x,y) 
     iterate i=0,1 reduce(sum)
     if i==0 then [uint8](x) else [uint8](10) end
     end
end)
