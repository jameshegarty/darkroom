package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require "test"
import "orion"

im a(x,y) : cropNone x+y*0 end
im b(x,y) : cropNone 0*a(x-10,y+5)+1*a(x+10,y+7) end

test(im(x,y) : crop(0,0,128,64),uint8 b(x-10,y) end)
