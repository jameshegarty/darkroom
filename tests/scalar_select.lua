package.path = package.path .. ";../src/?.lua;../src/?.t"
terralib.require "test"
import "orion"


im out(x,y) : crop(0,0,100,100),uint8[3] if x>50 then {255,0,0} else {0,255,255} end end

--  im out(x,y) orion.assert(1,1,out(x,y)<(th+1),out(x,y)) end


test(out)