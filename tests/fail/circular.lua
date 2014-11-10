(terralib.loadfile("test.t"))()
import "darkroom"

lol = darkroomSimple.tap(uint8,4)
-- attempt to test all the operators & types in the language
function gen(T,a)
  local drt = darkroom.type.fromTerraType(T)
  im a(x,y) [T](a+lol) end

  if drt:isArray() then
    return im(x,y) [uint8[3]](a) end
  else
    return im(x,y) [uint8](a) end
  end
end

local types = {uint8, int8, uint16, int16, uint32, int32, float, double,
               uint8[3], int8[3], uint16[3], uint32[3], int32[3], float[3], double[3]}

--local types = {uint8}

local tests = {}
for _,t in ipairs(types) do table.insert(tests, gen(t,testinput)) end
test(tests)
