(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) map i=-1,1 j=-1,1 reduce(sum) testinput(x*2+i,y*2+j)/[uint8](9) end end
im b(x,y) map i=-1,1 j=-1,1 reduce(sum) a(x*2+i,y*2+j)/[uint8](9) end end
im c(x,y) map i=-1,1 j=-1,1 reduce(sum) b(x*2+i,y*2+j)/[uint8](9) end end -- 8
im d(x,y) map i=-1,1 j=-1,1 reduce(sum) c(x*2+i,y*2+j)/[uint8](9) end end -- 16

im dup(x,y) (d(x/16-1,y/16)/[uint8](4))+(d(x/16+1,y/16)/[uint8](4))+(d(x/16,y/16-1)/[uint8](4))+(d(x/16,y/16+1)/[uint8](4)) end

test(im(x,y) testinput(x,y)-dup end)
