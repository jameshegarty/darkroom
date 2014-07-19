(terralib.loadfile("test.t"))()
import "darkroom"

local st = {}
for i=0,255 do table.insert(st, i*i/(255)) end

local square = darkroomSimple.tapLUT( uint8, 256, st )

test(im(x,y) square[testinput(x,y)] end)
