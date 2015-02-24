(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     W = (iterate i=0,1 A=darkroom.gatherColumn(testinput,i,1,0,1,-1,0) reduce(none)
          A[1]
--     W = (map i=-1,0 reduce(none)
--          testinput(x+i,y)
       end)
     in map i=0,1 reduce(sum) W[i] end
end)
