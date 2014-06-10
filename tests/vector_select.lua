terralib.require "test"
import "darkroom"

im out(x,y) [uint8[3]](orion.vectorSelect({x>10,x>20,x>50},{255,0,0},{0,255,255})) end

test(out)