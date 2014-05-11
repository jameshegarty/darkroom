terralib.require "test"
import "darkroom"

im out(x,y) orion.array3uint8( if x>50 then {255,0,0} else {0,255,255} end) end

test(out)