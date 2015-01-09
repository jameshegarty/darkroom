(terralib.loadfile("test.t"))()
import "darkroom"

if arg[1]=="cpu" then
   color = darkroomSimple.load("color.bmp")
else
   color = darkroom.input(uint8[3])
end

test(im(x,y) {color[0]+[uint8](10),color[1]+[uint8](20),color[2]+[uint8](30)} end,{{color,"color"}})
