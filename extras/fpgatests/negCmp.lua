(terralib.loadfile("test.t"))()
import "darkroom"

test(im(x,y) if [int8](testinput(x,y))>32 then [uint8](0) else testinput end end)
