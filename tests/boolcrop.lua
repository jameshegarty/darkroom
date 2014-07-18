terralib.require("test")
import "darkroom"

im stripe(x,y) darkroom.crop((x%4)==0) end
test(im(x,y) if stripe then testinput else [uint8](0) end end)
