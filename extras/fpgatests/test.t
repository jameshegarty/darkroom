import "darkroom"
fpga = terralib.require("fpga")
fpgaEstimate = terralib.require("fpgaEstimate")

testinput = darkroom.input(uint8)

function test(inast)
  assert(darkroom.ast.isAST(inast))

  if arg[1]=="est" then
    local est,pl = fpgaEstimate.compile({inast}, 640)
    io.output("out/"..arg[0]..".est.txt")
    io.write(est)
    io.close()
    io.output("out/"..arg[0]..".perlineest.txt")
    io.write(pl)
    io.close()
  else
    local v = fpga.compile( {{testinput,"uart"}}, {{inast,"uart"}}, 64, 64)
    io.output("out/"..arg[0]..".v")
    io.write(v)
    io.close()
  end
end