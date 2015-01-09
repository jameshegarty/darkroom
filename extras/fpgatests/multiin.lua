(terralib.loadfile("test.t"))()
import "darkroom"

if arg[1]=="cpu" then
   color = darkroomSimple.load("color.bmp")
else
   color = darkroom.input(uint8[3])
end

test(im(x,y) testinput(x,y)+color end,{{testinput,"frame_128"},{color,"color"}})
