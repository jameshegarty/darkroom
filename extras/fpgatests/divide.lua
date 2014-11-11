(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) ((testinput+[uint8](100))/([uint8](2))) / (testinput(x-10,y)+([uint8](1))) end
im b(x,y) ([uint16](a)*[uint16](100)) / ([uint16](testinput(x-10,y)+[uint16](3))) end
im c(x,y) ([int32](testinput*100)) / ([int32](b(x-10,y))+1) end

test(im(x,y) [uint8](b*100) end)
