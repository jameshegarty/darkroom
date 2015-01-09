(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     iterate i=-5,-5 A=darkroom.gatherColumn(testinput,((i+1)-1),1,-5,-4,0,0) reduce(sum)
       A[0]
          end
--     darkroom.gatherE(testinput,-5,0,-5,-5,0,0)
--     testinput(x-5,y)
end)
