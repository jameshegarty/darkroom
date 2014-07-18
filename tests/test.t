package.path = package.path .. ";../src/?.lua;../src/?.t"

local ffi = require("ffi")

-- LINE COVERAGE INFORMATION                                                                                                                              
if false then
  local CV = "out/coverageinfo."..arg[0]..arg[1]..".lua"
  print("CV",CV)
  local converageloader = loadfile(CV)
  local linetable = converageloader and converageloader() or {}
  local function dumplineinfo()
    local F = io.open(CV,"w")
    F:write("return {\n")
    for fk, fv in pairs(linetable) do
      F:write("['"..fk.."']={")
      for k,v in pairs(linetable[fk]) do
        F:write("["..k.."] = "..v..";\n")
      end
      F:write("},\n")
    end
    F:write("}\n")
    F:close()
  end
  local function debughook(event)
    local info = debug.getinfo(2,"Sl")
    if linetable[info.short_src]==nil then
      linetable[info.short_src]={}
    end

--    print("DB",info.short_src)
--        if info.short_src == "/Users/research/Documents/orion/darkroom.t" then
    linetable[info.short_src][info.currentline] = linetable[info.short_src][info.currentline] or 0
    linetable[info.short_src][info.currentline] = linetable[info.short_src][info.currentline] + 1
--        end
  end
  debug.sethook(debughook,"l")
    -- make a fake ffi object that causes dumplineinfo to be called when                                                                                  
    -- the lua state is removed                                                                                                                           
    ffi.cdef [[
                typedef struct {} __linecoverage;
              ]]
    ffi.metatype("__linecoverage", { __gc = dumplineinfo } )
    _G[{}] = ffi.new("__linecoverage")
end

import "darkroom"
terralib.require("darkroomDebug")
darkroomSimple = terralib.require("darkroomSimple")

-- call this with testscript.lua [runtype] [runfile]
-- runtype is int (interpreter), "allmat" (all materialized), "fast" (fast schedule)
-- "serial" (no vector)

-- dont run on this file
if arg[0]=="test.t" then os.exit() end

assert(#arg>0)

testinput = darkroomSimple.load(arg[1])

function test(inast)

  if darkroom.ast.isAST(inast) then inast = {inast} end

  local terra dosave(img: &Image, filename : &int8)
    img:save(filename)
  end

  local cores = tonumber(arg[2]) or 1
  local corest = ""
  if arg[2] then corest="."..arg[2] end

  local tprog = darkroomSimple.compile(inast,{debug=true, verbose=true, printruntime=true, cores=cores})

  local res = pack(unpacktuple(tprog()))
  for k,v in ipairs(res) do
    print(v)
    local st = ""
    if k>1 then st = "."..k end
      dosave(v,"out/"..arg[0]..st..corest..".bmp")
    end

end