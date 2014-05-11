terralib.require "test"
import "darkroom"

im b(x,y) 0 end

print("B",b)

for i=-1,1 do
  for j=-1,1 do
    im b(x,y) b(x,y) + testinput(x+i,y+j)/9 end
  end
end

im b(x,y) orion.uint8(b(x,y)) end

test(b)