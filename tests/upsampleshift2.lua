(terralib.loadfile("test.t"))()
import "darkroom"

-- this should have two pixels of black on the left,
-- four pixels of black on the bottom
-- (0/2)-1 = -1, (1/2)-1 = -1
test(im(x,y) --phase = {x%2,y%2}
  testinput((x/2)-1,y/2-2)
--  if x%2==0 then testinput(x/2,y/2) else (testinput(x/2,y/2)/[uint8](2))+(testinput((x/2)-1,y/2)/[uint8](2)) end
--  if y%2==0 then testinput(x/2,y/2) else (testinput(x/2,y/2)/[uint8](2))+(testinput((x/2),(y/2)+1)/[uint8](2)) end
 end)
