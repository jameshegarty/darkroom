(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) 
     map i=-1,0 reduce(sum)
     A={testinput(x+i,y-1),testinput(x+i,y)}
     in A[0]+A[1]
     end
end)
