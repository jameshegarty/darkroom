(terralib.loadfile("test.t"))()
import "darkroom"

if arg[1]=="cpu" then
   color = darkroomSimple.load("color.bmp")
else
   color = darkroom.input(uint8[3])
end

im color(x,y) {color[0],color[1],color[2],[uint8](0)} end
test(im(x,y) map i=-1,1 j=-1,1 reduce(sum) color(x+i,y+j) end  end,{{color,"color"}})
