package.path = package.path .. ";../src/?.lua;../src/?.t"

terralib.require "test"
import "orion"

-- test that chains of translates work
im a(x,y):cropNone testinput(x-1,y) + testinput(x+1,y) end
im b(x,y):cropNone a(x+1,y) end
im c(x,y):cropNone b(x+1,y) end
im d(x,y):cropSame c(x+1,y) end

test(d)
