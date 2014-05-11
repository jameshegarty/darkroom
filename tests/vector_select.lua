terralib.require "test"
import "darkroom"

im out(x,y) orion.array3uint8(orion.vectorSelect({x>10,x>20,x>50},{255,0,0},{0,255,255})) end

test(out)