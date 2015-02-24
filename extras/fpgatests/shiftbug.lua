(terralib.loadfile("test.t"))()
import "darkroom"

windowRadius = 2

local frame1 = testinput

local im Fx(x,y) frame1(x+1,y)-frame1(x-1,y) end
local im Fy(x,y) frame1(x,y+1)-frame1(x,y-1) end


local im Atemp0(x,y)
map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
        Fx(x+wx,y+wy)*Fx(x+wx,y+wy)
        end
end

local im Atemp1(x,y)
map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
        Fx(x+wx,y+wy)*Fy(x+wx,y+wy)
        end
end

test(im ret(x,y) {[uint8](Atemp0),[uint8](Atemp1),[uint8](Atemp0)} end)
