package.path = package.path .. ";../src/?.lua;../src/?.t"
terralib.require "test"
import "orion"

th = 100

im out(x,y) if testinput(x,y)>orion.uint8(th) then orion.uint8(0) else testinput(x,y) end end

--  im out(x,y) orion.assert(1,1,out(x,y)<(th+1),out(x,y)) end


test(out)