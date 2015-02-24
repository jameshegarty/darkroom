(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     iterate i=-1,0 A=darkroom.gatherColumn(testinput,i,1,-1,0,-3,0) reduce(sum)
       A[0]  -- this will be y=-3
     end
end)
