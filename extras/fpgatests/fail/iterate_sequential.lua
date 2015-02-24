(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     first = (iterate i=-1,0 A=darkroom.gatherColumn(testinput,i,1,-1,0,-1,0) reduce(sum)
       A[0]+A[1]
       end)
     in (iterate i=-1,0 A=darkroom.gatherColumn(testinput,i,1,-1,0,-1,0) reduce(sum)
       A[0]+A[1]+first
       end)
end)
