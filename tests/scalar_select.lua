terralib.require "test"
import "darkroom"

im out(x,y) : crop(0,0,100,100),uint8[3] if x>50 then {255,0,0} else {0,255,255} end end

test(out)