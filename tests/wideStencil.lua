terralib.require("test")
import "darkroom"

local const = darkroomSimple.load("frame_1024.bmp")

im a(x,y)  orion.float32(const(x,y)) end
im b(x,y) a(x-20,y-1)+a(x+20,y+1) end
im c(x,y) b(x-20,y-1)+b(x+20,y+1) end
im d(x,y) c(x-20,y-1)+c(x+20,y+1) end
im e(x,y) d(x-20,y-1)+d(x+20,y+1) end
im f(x,y) e(x-20,y-1)+e(x+20,y+1) end

test(f)
