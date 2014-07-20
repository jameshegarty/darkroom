import "darkroom"
terralib.require("darkroomDebug")
darkroomSimple = terralib.require("darkroomSimple")
dpda = terralib.require("dpda")

if arg[0]=="test.t" then os.exit() end

assert(#arg>0)

testinput = darkroomSimple.load(arg[1])

function test(inast)
  if darkroom.ast.isAST(inast) then
    inast = {inast}
  end

  
  local inputs = {}
  local taps = {}

  local out_kernels, out_config = dpda.compile(inputs, inast, taps)

  local a0 = arg[0]:sub(13)

  local file = io.open("out/"..a0..".yml","w")
  file:write(out_kernels)
  file:close()
  
  local rfile = io.open("out/"..a0.."_run.yml","w")
  rfile:write(out_config)
  rfile:close()
end