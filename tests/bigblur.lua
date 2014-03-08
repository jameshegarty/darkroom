package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require "test"
import "orion"

im b(x,y) : cropNone 0 end

print("B",b)

for i=-1,1 do
  for j=-1,1 do
    im b(x,y) : cropNone b(x,y) + testinput(x+i,y+j)/9 end
  end
end

im b(x,y) : cropSame,uint8 b(x,y) end

test(b)