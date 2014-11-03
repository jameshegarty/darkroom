import "darkroom"
fpga = terralib.require("fpga")
fpgaEstimate = terralib.require("fpgaEstimate")
darkroomSimple = terralib.require("darkroomSimple")
terralib.require("image")

if arg[1]=="cpu" then
  testinput = darkroomSimple.load(arg[2])
else
  testinput = darkroom.input(uint8)
end



function test(inast, inputList)


  print("TEST",arg[1],arg[2])
  if arg[1]=="est" then
    local cpuinast
    if darkroom.ast.isAST(inast) then 
      cpuinast = {inast} 
    else
      cpuinast={}
      for k,v in ipairs(inast) do table.insert(cpuinast, v[1]) end
    end

    local est,pl = fpgaEstimate.compile(cpuinast, 640)
    io.output("out/"..arg[0]..".est.lua")
    io.write(est)
    io.close()
    io.output("out/"..arg[0]..".perlineest.txt")
    io.write(pl)
    io.close()
  elseif arg[1]=="build" then
    local hwinputs = inputList
    if hwinputs==nil then hwinputs={{testinput,"uart","frame_128.bmp"}} end
    local hwoutputs = inast
    if darkroom.ast.isAST(hwoutputs) then
      hwoutputs = {{inast,"uart"}}
    end

    local v, metadata = fpga.compile(hwinputs, hwoutputs, 128, 64, fpga.util.deviceToOptions(arg[3]))
    local s = string.sub(arg[0],1,#arg[0]-4)
    io.output("out/"..s..".v")
    io.write(v)
    io.close()

    fpga.util.writeMetadata("out/"..s..".metadata.lua", metadata)

  else
    local cpuinast
    if darkroom.ast.isAST(inast) then 
      cpuinast = {inast} 
    else
      cpuinast={}
      for k,v in ipairs(inast) do table.insert(cpuinast, v[1]) end
    end

    local terra dosave(img: &Image, filename : &int8)
      img:save(filename)
      img:free()
    end

    local tprog = darkroomSimple.compile(cpuinast,{debug=true, verbose=true, printruntime=true})

    local res = pack(unpacktuple(tprog()))
    for k,v in ipairs(res) do
      print(v)
      local st = ""
      if k>1 then st = "."..k end
      dosave(v,"out/"..arg[0]..st..".bmp")
    end
  end
end