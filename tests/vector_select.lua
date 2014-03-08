package.path = package.path .. ";../src/?.lua;../src/?.t"
terralib.require "test"
import "orion"


im out(x,y) : crop(0,0,100,100),uint8[3] orion.vectorSelect({x>10,x>20,x>50},{255,0,0},{0,255,255}) end

--  im out(x,y) orion.assert(1,1,out(x,y)<(th+1),out(x,y)) end


test(out)