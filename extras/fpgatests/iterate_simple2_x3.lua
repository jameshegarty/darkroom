(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
--     iterate i=-1,1 A=darkroom.gatherColumn(testinput,i,1,-1,1,0,0) reduce(sum)
     iterate i=-2,0 A=darkroom.gatherColumn(testinput,i,1,-2,0,0,0) reduce(sum)
--     iterate i=-1,0 A=darkroom.gatherColumn(testinput,i,1,-1,0,0,0) reduce(sum)
       A[0]
     end
end)
