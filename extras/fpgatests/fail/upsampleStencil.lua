(terralib.loadfile("test.t"))()
import "darkroom"

local testinput = im(x,y) testinput(x*2,y*2) end

test(im(x,y) 
     phaseX = x and [uint8](1)
     phaseY = y and [uint8](1)
     in if phaseX==0 and phaseY==0 then testinput(x/2,y/2) else
     if phaseX==1 and phaseY==0 then (testinput(x/2,y/2)>>[uint8](1))+(testinput((x/2)+1,y/2)>>[uint8](1)) else
        if phaseX==0 and phaseY==1 then (testinput(x/2,y/2)>>[uint8](1))+(testinput((x/2),(y/2)+1)>>[uint8](1)) else
     (testinput(x/2,y/2)>>[uint8](2))+(testinput((x/2)+1,y/2)>>[uint8](2))+(testinput(x/2,(y/2)+1)>>[uint8](2))+(testinput((x/2)+1,(y/2)+1)>>[uint8](2))
     end end end
 end)
