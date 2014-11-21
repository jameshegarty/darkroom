(terralib.loadfile("test.t"))()
import "darkroom"

-- this should have dark pixels on the bottom/left, but not top/right.
-- b/c 31*2+1 = 63 (inside the crop), but 0*2-1=-1 (outside the crop)

test(im(x,y) (testinput(x*2-1,y*2)>>[uint8](2))+(testinput(x*2+1,y*2)>>[uint8](2))+(testinput(x*2,y*2-1)>>[uint8](2))+(testinput(x*2,y*2+1)>>[uint8](2)) end)
