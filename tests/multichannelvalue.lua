(terralib.loadfile("test.t"))()
import "darkroom"

-- tables containing only image functions should become multichannel images
a = {im(x,y) testinput(x,y)+[uint8](10) end,
im(x,y) testinput(x,y)+[uint8](60) end,
im(x,y) testinput(x,y)+[uint8](100) end}

test(im(x,y) [a] end)
