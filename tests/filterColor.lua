(terralib.loadfile("test.t"))()
import "darkroom"

I = darkroomSimple.load("color.bmp")

local cores = tonumber(arg[2]) or 1
local corest = ""
if arg[2] then corest=".c"..arg[2] end
TEST_OUTPUT_OVERRIDE = "out/filterColor.lua"..corest..".csv"

test(im(x,y) darkroom.filter(x%32==0 and y%32==0,I) end)
