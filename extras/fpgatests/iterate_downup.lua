(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput(x*2,y*2) end
im a(x,y) a(x/2,y/2) end
test(im(x,y) 
     iterate i=-1,0 A=darkroom.gatherColumn(a,i,1,-1,0,-1,0) reduce(sum)
       map j=0,1 reduce(sum) A[j] end
     end
end)
