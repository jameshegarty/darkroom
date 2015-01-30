(terralib.loadfile("test.t"))()
import "darkroom"

-- attempt to test all the operators & types in the language
function gen(T,a)
  local drt = darkroom.type.fromTerraType(T)
  im a(x,y) [T](a) end

  if drt:isArray() then
    im a(x,y) [T](if a[0]==3 then a+1 else a+2 end) end
  else
    im a(x,y) [T](if a==3 then a+1 else a+2 end) end
  end

  im a(x,y) [T](map i=-1,1 reduce(sum) a(x,y)/3 end) end -- mapreduce var unused
  im a(x,y) [T](map i=-1,1 reduce(max) a(x,y)/3 end) end -- mapreduce var unused
  im a(x,y) [T](map i=-1,1 reduce(min) a(x,y)/3 end) end -- mapreduce var unused

  local arg1, arg2
  if drt:isArray() then
    im arg1(x,y) (map i=-1,1 reduce(argmin) a[0](x,y)/3 end) end -- mapreduce var unused
    im arg2(x,y) (map i=-1,1 reduce(argmax) a[0](x,y)/3 end) end -- mapreduce var unused
  else
    im arg1(x,y) (map i=-1,1 reduce(argmin) a(x,y)/3 end) end -- mapreduce var unused
    im arg2(x,y) (map i=-1,1 reduce(argmax) a(x,y)/3 end) end -- mapreduce var unused
  end

  im a(x,y) [T](a+arg1[0]+arg2[0]) end

  local anv = a
  if drt:isArray() then anv = im(x,y) a[0] end  end

  im a(x,y) [T]( switch anv
                 case 45 -> a+3
                 case 47 -> a+2
                 default -> a+1
                 end) end

  im a(x,y) darkroom.print(a) end

  local anv = a
  if drt:isArray() then anv = im(x,y) a[0] end  end

--  im a(x,y) darkroom.assert(anv==0, 42, a > 512) end

  if drt:isArray() then
    im a(x,y) [T](darkroom.vectorSelect(a==3,a+1,a+2)) end
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
