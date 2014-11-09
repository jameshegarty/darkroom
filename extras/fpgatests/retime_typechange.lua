(terralib.loadfile("test.t"))()
import "darkroom"

-- in the middle of this, there is a variable that's being retimed, where the type of its
-- consumer is different than its producer. The type must be set by the producer or it will fail.

local out = im(x,y) {testinput, testinput, (testinput+testinput)} end
test(im(x,y) [uint8[3]]( darkroom.vectorSelect(out>[uint8](255),[uint8[3]](255),out) ) end)
