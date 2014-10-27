(terralib.loadfile("test.t"))()
import "darkroom"

local cores = tonumber(arg[2]) or 1
local corest = ""
if arg[2] then corest="."..arg[2] end
TEST_OUTPUT_OVERRIDE = "out/filter.lua"..corest..".csv"
test(im(x,y) darkroom.filter(x%32==0 and y%32==0,testinput) end)
