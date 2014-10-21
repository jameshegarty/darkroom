(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) phase = {x%2,y%2}
     in if darkroom.arrayAnd(phase=={0,0}) then testinput(x/2,y/2) else
     if darkroom.arrayAnd(phase=={1,0}) then (testinput(x/2,y/2)/[uint8](2))+(testinput((x/2)-1,y/2)/[uint8](2)) else
     if darkroom.arrayAnd(phase=={0,1}) then (testinput(x/2,y/2)/[uint8](2))+(testinput((x/2),(y/2)-1)/[uint8](2)) else
     (testinput(x/2,y/2)/[uint8](4))+(testinput((x/2)-1,y/2)/[uint8](4))+(testinput(x/2,(y/2)-1)/[uint8](4))+(testinput((x/2)-1,(y/2)-1)/[uint8](4))
     end end end
 end)
