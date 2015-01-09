import "darkroom"
(terralib.loadfile("test.t"))()

test(im(x,y) [uint8[0]](testinput(x,y)) end)