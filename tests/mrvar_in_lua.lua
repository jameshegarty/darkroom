(terralib.loadfile("test.t"))()
import "darkroom"

function mrf(i,j)
  return im(x,y) testinput(x+i,y+j) end
end

-- synth something that selects one of the pixels in a 3x3 area
test(im(x,y)
     [uint8](map i=-10,10 j=-1,1 reduce(sum) [mrf(i,j)] end)
end)