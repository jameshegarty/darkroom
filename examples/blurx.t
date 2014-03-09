--a = orion.load("300d.bmp")
--im a(x,y) : float32 a(x,y) end
import "orion"

a = orion.load("color.bmp")
im a(x,y) : float32[3] a(x,y) end

--im blurx(x,y) (a(x-1,y)+a(x,y)+a(x+1,y))/3 end
--im blurx(x,y) : uint8[3] (blurx(x-1,y)+blurx(x,y)+blurx(x+1,y))/3 end
im blurx(x,y) : uint8[3] 
map i=-5,5 reduce(sum) a(x+i,y)/11 end
end

if arg[1]=="conv" then
  local yfile,yrfile = orion.compile({blurx},{verbose=false,debug=false,platform="convolution"})

  local file = io.open("out/blurx.yml","w")
  file:write(yfile)
  file:close()
  
  local rfile = io.open("out/blurx_run.yml","w")
  rfile:write(yrfile)
  rfile:close()
else
  tprog,model = orion.compile({blurx},
                              {
                                debug=false, 
                                verbose=false,
                                printruntime=true,
                                looptimes=50})
  
  terra doit()
    var res = tprog()
    res:save("out/blurx.bmp")
  end
  
  doit()
  
--  print("model total",model.total)
--  for k,v in pairs(model.nodes) do
--    print(k,v)
--  end
end