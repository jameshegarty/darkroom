(terralib.loadfile("test.t"))()
import "darkroom"


windowRadius = 2

local frame1Original = darkroomSimple.load("left0224.bmp")
frame1 = im(x,y) frame1Original(x*2,y*2)[0] end
--frame1 = im(x,y) frame1Original[0] end
--local result1 = makeLK( frame1, frame1 )

  local im A(x,y)
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
    frame1(x+wx,y+wy)
    end 
  end

--  local Ainv = invert2x2(A)

  local im det(x,y) 1/A(x,y) end
 im Ainv(x,y) det*A end

--Ainv:save("out/lucaskanade.bmp", {cores=1})
test(Ainv)