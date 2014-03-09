package.path = package.path .. ";../src/?.lua;../src/?.t"

local ffi = require("ffi")

-- LINE COVERAGE INFORMATION                                                                                                                              
if true then
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
--        if info.short_src == "/Users/research/Documents/orion/orion.t" then
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

import "orion"
-- call this with testscript.lua [runtype] [runfile]
-- runtype is int (interpreter), "allmat" (all materialized), "fast" (fast schedule)
-- "serial" (no vector)

-- dont run on this file
if arg[0]=="test.t" then os.exit() end

assert(#arg>0)

if arg[1]=="conv" and arg[2]==nil then
  testinput = orion.image(orion.type.uint(12))
else
  testinput = orion.load(arg[2])
end

function test(inast)

  if orion.ast.isAST(inast) then inast = {inast} end

  local terra dosave(img: &Image, filename : &int8)
    img:save(filename)
  end


  if arg[1]=="default" or arg[1]=="mat" or arg[1]=="allmat" or arg[1]=="alllb" then
    local tprog

    if arg[1]=="default" then
      tprog = orion.compile(inast,{debug=true, verbose=true, printruntime=true})
    elseif arg[1]=="mat" then
      tprog = orion.compile(inast,{schedule="materialize",debug=true, verbose=true, printruntime=true})
    elseif arg[1]=="allmat" then
      tprog = orion.compile(inast,{schedule="materializeall",debug=true, verbose=true, printruntime=true})
    elseif arg[1]=="alllb" then
      tprog = orion.compile(inast,{schedule="linebufferall",debug=true, verbose=true, printruntime=true})
    else
      assert(false)
    end

    local res = pack(unpacktuple(tprog()))
    for k,v in ipairs(res) do
      print(v)
      local st = ""
      if k>1 then st = "."..k end
      dosave(v,"out/"..arg[0]..st.."."..arg[1]..".bmp")
    end

  elseif arg[1]=="conv" then
    local resultYAML, runYAML = orion.compile(inast,{platform="convolution",debug=true, verbose=true, printruntime=true})
    --print(resultYAML)
    local file = io.open("out/"..arg[0]..".yml","w")
    file:write(resultYAML)
    file:close()

    local rfile = io.open("out/"..arg[0].."_run.yml","w")
    rfile:write(runYAML)
    rfile:close()
  else
    print("Error, unknown runtype ",arg[1])
  end

end