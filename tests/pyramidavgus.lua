(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) map i=-1,1 j=-1,1 reduce(sum) testinput(x*2+i,y*2+j)/[uint8](9) end end
im b(x,y) map i=-1,1 j=-1,1 reduce(sum) a(x*2+i,y*2+j)/[uint8](9) end end
im c(x,y) map i=-1,1 j=-1,1 reduce(sum) b(x*2+i,y*2+j)/[uint8](9) end end -- 8
--im d(x,y) map i=-1,1 j=-1,1 reduce(sum) c(x*2+i,y*2+j)/[uint8](9) end end -- 16


function boxUpsample(inp)
  return  im(x,y) phase = {x%2,y%2}
     in (if darkroom.arrayAnd(phase=={0,0}) then inp(x/2,y/2) else
     if darkroom.arrayAnd(phase=={1,0}) then (inp(x/2,y/2)/[uint8](2))+(inp((x/2)+1,y/2)/[uint8](2)) else
     if darkroom.arrayAnd(phase=={0,1}) then (inp(x/2,y/2)/[uint8](2))+(inp((x/2),(y/2)+1)/[uint8](2)) else
     (inp(x/2,y/2)/[uint8](4))+(inp((x/2)+1,y/2)/[uint8](4))+(inp(x/2,(y/2)+1)/[uint8](4))+(inp((x/2)+1,(y/2)+1)/[uint8](4))
     end end end) end --*[uint8](0)+[uint8](y%2) end
end

test(boxUpsample(c))
