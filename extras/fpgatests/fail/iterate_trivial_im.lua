(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     iterate i=0,0 A=darkroom.gatherColumn(testinput,i,1,0,0,0,0) reduce(sum)
       A[0]
     end
end)
