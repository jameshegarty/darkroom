(terralib.loadfile("test.t"))()
import "darkroom"

if arg[1]=="cpu" then
   color = darkroomSimple.load("color.bmp")
else
   color = darkroom.input(uint8[3])
end

-- expand to 4 bytes to work around LB limitation
im color4(x,y) {color[0],color[1],color[2],[uint8](0)} end
im A(x,y) map i=-1,1 j=-1,1 reduce(sum) color4(x+i,y+j) end  end

test(im(x,y) {A[0],A[1],A[2]} end,{{color,"color"}})
