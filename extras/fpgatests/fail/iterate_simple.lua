(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     iterate i=-1,0 A=darkroom.gatherColumn(testinput,i,1,-1,0,-1,0) reduce(sum)
       map j=0,1 reduce(sum) A[j] end
     end
end)
