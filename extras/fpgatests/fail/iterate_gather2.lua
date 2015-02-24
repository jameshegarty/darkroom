(terralib.loadfile("test.t"))()
import "darkroom"

function fastGather(inp,xcoord,ycoord,N)
   return im(x,y) iterate i=0,0 A=darkroom.gatherColumn(inp,xcoord,1,-N,N,-N,N) reduce(sum)
   map j=-N,N reduce(sum) if j==ycoord then A[j+N] else [uint8](0) end end
   end end
end

im xcoord(x,y) testinput and [uint8](7) end
im ycoord(x,y) testinput and [uint8](7) end

--im xcoord(x,y) [int32](7) end
--im ycoord(x,y) [int32](-3) end

test(fastGather(testinput,xcoord,ycoord,8))
