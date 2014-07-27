(terralib.loadfile("test.t"))()
import "darkroom"

-- attempt to test all the operators & types in the language
function gen(T,a)
  local drt = darkroom.type.fromTerraType(T)
  im a(x,y) [T](a) end
  im a(x,y) [T](darkroom.pow(a,2)) end
  im a(x,y) [T](darkroom.min(a+3,a+2,a+5)) end
  im a(x,y) [T](darkroom.crop(a)) end
  im a(x,y) [T](darkroom.max(a+3,a+2,a+5)) end
  im a(x,y) [T](darkroom.sum(a+3,a+2,a+5)) end

  -- binops
  im a(x,y) [T](a(x-1-2-3+4,y+4-1+3+4)+a(x+40,y+30)) end
  im a(x,y) [T]((a+a) - a) end
  im a(x,y) [T](a * a) end
  im a(x,y) [T](a % 64) end
  im a(x,y) [T](a / 2) end

  if drt:baseType():isFloat()==false then
    im a(x,y) [T](a >> 2) end
    im a(x,y) [T](a << 2) end
    im a(x,y) [T](a ^ (a+1)) end -- xor
    im a(x,y) [T](a and (a+1)) end
    im a(x,y) [T](a or (a+1)) end
  end

  if drt:isArray() then im a(x,y) [T](darkroom.dot(a,a)) end end

  -- logical
  local im la(x,y) (a < 4) and (a > 3) end
  local im lb(x,y) not (a > 3) end

  if drt:isArray() then
    im a(x,y) [T](if la[0] or lb[0] then a else a+1 end) end
  else
    im a(x,y) [T](if la or lb then a else a+1 end) end
  end

  -- unary
  im a(x,y) [T](darkroom.floor(a)) end
  im a(x,y) [T](darkroom.ceil(a)) end
  im a(x,y) [T](darkroom.abs(a)) end

  if drt:isFloat() then
    im a(x,y) [T](darkroom.cos(a)) end
    im a(x,y) [T](darkroom.sin(a)) end
    im a(x,y) [T](darkroom.exp(a)) end
  end

  if drt:isArray() then
    return im(x,y) [uint8[3]](a) end
  else
    return im(x,y) [uint8](a) end
  end
end

local types = {uint8, int8, uint16, int16, uint32, int32, float, double,
               uint8[3], int8[3], uint16[3], uint32[3], int32[3], float[3], double[3]}

local tests = {}
for _,t in ipairs(types) do table.insert(tests, gen(t,testinput)) end
test(tests)
