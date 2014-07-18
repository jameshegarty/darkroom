terralib.require("test")
import "darkroom"

-- attempt to test all the operators & types in the language
function gen(T,a)
  local drt = orion.type.fromTerraType(T)
  im a(x,y) [T](a) end
  im a(x,y) [T](orion.pow(a,2)) end
  im a(x,y) [T](orion.min(a+3,a+2,a+5)) end
  im a(x,y) [T](orion.crop(a)) end
  im a(x,y) [T](orion.max(a+3,a+2,a+5)) end
  im a(x,y) [T](orion.sum(a+3,a+2,a+5)) end

  -- binops
  im a(x,y) [T](a(x-1-2-3+4,y+4-1+3+4)+a(x+40,y+30)) end
  im a(x,y) [T]((a+a) - a) end
  im a(x,y) [T](a * a) end
  im a(x,y) [T](a % 64) end
  im a(x,y) [T](a / 2) end
  im a(x,y) [T](a >> 2) end
  im a(x,y) [T](a << 2) end
  im a(x,y) [T](a and (a+1)) end
  im a(x,y) [T](a or (a+1)) end
  im a(x,y) [T](a ^ (a+1)) end -- xor
  if drt:isArray() then im a(x,y) [T](orion.dot(a,a)) end end

  -- logical
  im la(x,y) (a < 4) and (a > 3) end
  im lb(x,y) not (a > 3) end
  im a(x,y) [T](if la or lb then a else a+1 end) end

  -- unary
  im a(x,y) [T](orion.floor(a)) end
  im a(x,y) [T](orion.ceil(a)) end
  im a(x,y) [T](orion.abs(a)) end

  if drt:isFloat() then
    im a(x,y) [T](orion.cos(a)) end
    im a(x,y) [T](orion.sin(a)) end
    im a(x,y) [T](orion.exp(a)) end
  end

  im a(x,y) [T](if a==3 then a+1 else a+2 end) end
  im a(x,y) [T](orion.vectorSelect(a==3,a+1,a+2)) end

  im a(x,y) [T](map i=-1,1 reduce(sum) a(x,y)/3 end) end -- mapreduce var unused
  im a(x,y) [T](map i=-1,1 reduce(max) a(x,y)/3 end) end -- mapreduce var unused
  im a(x,y) [T](map i=-1,1 reduce(min) a(x,y)/3 end) end -- mapreduce var unused
  im a(x,y) [T](map i=-1,1 reduce(argmin) a(x,y)/3 end) end -- mapreduce var unused
  im a(x,y) [T](map i=-1,1 reduce(argmax) a(x,y)/3 end) end -- mapreduce var unused

  im a(x,y) [T]( switch A
                 case 45 -> 46
                 case 47 -> 32
                 default -> A+1
                 end) end

  im a(x,y) darkroom.print(a) end

  im a(x,y) darkroom.assert(a, 42, a > 512) end

  if drt:isArray() then
    return im(x,y) [uint8[3]](a) end
  else
    return im(x,y) [uint8](a) end
  end
end

local types = {uint8, int8, uint16, int16, uint32, int32, float, double,
               uint8[3]}
local tests = {}
for _,t in ipairs(types) do table.insert(tests, gen(t,testinput)) end
test(tests)
