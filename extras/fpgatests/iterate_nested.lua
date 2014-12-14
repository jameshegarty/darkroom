(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     iterate i=-1,0 A=darkroom.gatherColumn(testinput,i,1,-1,0,-1,0) reduce(sum)
       iterate j=-1,0 B=darkroom.gatherColumn(testinput,i,1,-1,0,-1,0) reduce(sum)
         A[0]+A[1]+B[0]+B[1]
       end
     end
end)
