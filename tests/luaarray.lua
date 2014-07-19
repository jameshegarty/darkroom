(terralib.loadfile("test.t"))()
import "darkroom"

local T = {1,2,1, --4
2,3,2, --7
4,5,6} --15

idx = 9
im a(x,y) 
  [uint8](map i=-1,1 j=-1,1 reduce(sum) (testinput(x,y)*T[idx-1])/(9*[T[idx]]) end)
end

test(im(x,y) 
     [uint8](map i=-1,1 j=-1,1 reduce(sum) (a(x+i,y+j)*T[(j+1)*3+(i+1)])/(26) end)
end)
