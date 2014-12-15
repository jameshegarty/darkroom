(terralib.loadfile("test.t"))()
import "darkroom"

function fastGather(inp,xcoord,ycoord,N)
   return im(x,y) iterate i=0,0 A=darkroom.gatherColumn(inp,xcoord,1,-N,N,-N,N) reduce(sum)
   map j=-N,N reduce(sum) if j==ycoord then A[j+N] else [uint8](0) end end
   end end
end

test(im(x,y) 
--     xcoord = (testinput and [uint8](7))
--     ycoord = (testinput(x-1,y)  and [uint8](7))
     xcoord = 7
     ycoord = -3
     in [fastGather(testinput,xcoord,ycoord,8)]
end)
