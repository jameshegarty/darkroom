(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) testinput-[uint8](100) end
im b(x,y) testinput>>[uint8](2) end
test(im(x,y) darkroom.sum(a,b,[uint8](10))+darkroom.max(a,b,[uint8](5)) end)
