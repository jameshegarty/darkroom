(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) ((testinput/([uint8](100)))/testinput) end
im b(x,y) ([uint16](a))/([uint16](testinput+[uint16](3))) end
im c(x,y) ([uint32](testinput))/([uint32](b)) end

test(c)
