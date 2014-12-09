(terralib.loadfile("test.t"))()
import "darkroom"

darkroomSimple.setImageSize(128,64)

-- This tests whether we're doing an arithmatic shift (correct), or a logical shift (incorrect)
test(im(x,y) [uint8]([int32]([int32](67)*[int32](-43))>> [uint8](25)) end)
