(terralib.loadfile("test.t"))()
import "darkroom"

th = 100
im out(x,y) if testinput(x,y)>[uint8](th) then [uint8](0) else testinput(x,y) end end

test(out)