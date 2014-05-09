terralib.require "test"
import "darkroom"

th = 100
im out(x,y) if testinput(x,y)>orion.uint8(th) then orion.uint8(0) else testinput(x,y) end end

test(out)