(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     W = testinput(x-1,y)+testinput(x+1,y)
     in (iterate i=-1,0 A=darkroom.gatherColumn(testinput,i,1,-1,0,-1,0) reduce(sum)
         W+A[0]
      end)+W
end)
