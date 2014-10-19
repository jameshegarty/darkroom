(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) switch {x%2,y%2}
     case {0,0} -> testinput(x/2,y/2)
     case {1,0} -> (testinput(x/2,y/2)/2)+(testinput((x/2)-1,y/2)/2)
     case {0,1} -> (testinput(x/2,y/2)/2)+(testinput((x/2),(y/2)-1)/2)
     default -> (testinput(x/2,y/2)/2)+(testinput((x/2)-1,y/2)/2)+(testinput(x/2,(y/2)-1)/2)+(testinput((x/2)-1,(y/2)-1)/2)
     end
 end)
