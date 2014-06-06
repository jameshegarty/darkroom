import "darkroom"

fusedIter = 5
unfusedIter = 10

function tofloat(input)
    assert(input)
    return im(x,y) : float32[3] input(x,y)/(254.5 + .5) end
end

function invert(input) return im(x,y) 1.0 - input(x,y) end end

function tobyte(input)
    assert(input)
    return im(x,y) : uint8[3] 
        let t = input(x,y)*(254.5 + .5) 
        in {if t[0] > 255.0 then 255.0 else t[0] end,
           if t[1] > 255.0 then 255.0 else t[1]end,
           if t[2]  > 255.0 then 255.0 else t[2] end}
    end

end

function convolve(K,input)
    local N2 = math.floor(math.sqrt(#K))
    assert(N2 % 2 == 1 and N2*N2 == #K)
    local N = math.floor(N2/2)

    local T = {}
    for i= -N, N do for j = -N, N do
        local idx = (-j + N)*N2 + (i + N) + 1
        print(i,j,K[idx])
        table.insert(T,im out(x,y) : cropNone  input(x+i,y+j) * [K[idx]] end)
    end end

    local fin = orion.sum(T[1],unpack(T))
    return im(x,y) : cropSame fin end
--  return im(x,y) : cropSame out(x,y) end
end

local function normalizekernel(K)
    local weight = 0
    for i = 1,#K do
        weight = weight + K[i]
    end
    for i = 1,#K do
        K[i] = K[i] / weight
    end
    return K
end


local function dumpk(K)
    local N = math.floor(math.sqrt(#K))
    local idx = 1
    for i = 1,N do
        for j = 1,N do
            io.write(K[idx] .." ")
            idx = idx + 1
        end
        io.write("\n")
    end
end
    
local function flip(K)
    local N = math.floor(math.sqrt(#K))
    assert(N % 2 == 1 and N*N == #K)
    local K2 = {}
    for i = 1,#K do
        K2[#K-i+1] = K[i]
    end
--    dumpk(K)
--    dumpk(K2)
   return K2
end

local function deconv(K,observed,latent_est, N)
    local Khat = flip(K)
    for i = 1,N do
        local est_conv = convolve(K,latent_est)
        local relative_blur = im(x,y) observed(x,y) / est_conv(x,y) end
        local error_est = convolve(Khat,relative_blur)
        latent_est = im(x,y) latent_est(x,y)*error_est(x,y) end
    end
    return latent_est
end

-- basically all the mass is within 3*variance
function gaussian(distSquared, variance)
  return  math.exp(-distSquared/(2*variance))
end

function gaussianKernel(radius)
  local K = {}

  for y=-radius, radius do
    for x=-radius, radius do
      table.insert(K,gaussian(x*x+y*y,radius/1.5))
    end
  end

  print(radius,#K)
  assert(#K == math.pow(radius*2+1,2))
  return K
end

local K = normalizekernel {1,0,0,
                           0,1,0,
                           0,0,1}

--[=[
K = normalizekernel(gaussianKernel(5))

-- handshake kernel
K = normalizekernel {1,1,0, 0, 0,0,0,
                     0,1,1, 0, 0,0,0,
                     0,0,1, 0, 0,0,0,

                     0,0,1, 1, 0,0,0,

                     0,0,0, 0, 1,1,1,
                     0,0,0, 0, 0,1,1,
                     0,0,0, 0, 0,0,1}
]=]

local rgbf = orion.type.array(orion.type.float(32),3)

if arg[1]=="conv" then
  local W = 256
  local H = 256
  
  local observed = orion.image(rgbf,W,H)
  local latent_est = orion.image(rgbf,W,H)

  local convoutput = deconv(K,observed,latent_est,5)
  local yfile, yrfile = orion.compile( {convoutput},
                                  {platform = "convolution"})

  local file = io.open("out/deconvlucy.yml","w")
  file:write(yfile)
  file:close()
  
  local rfile = io.open("out/deconvlucy_run.yml","w")
  rfile:write(yrfile)
  rfile:close()

else
  local input = orion.load("color.bmp")                           
  --input:save("input.bmp")
  
  tobyte(convolve(K,tofloat(input))):save("out/deconvlucy_blurred.bmp")

  local inputFile = arg[1] or "out/deconvlucy_blurred.bmp"
  terra getWH()
    var f0 : Image
    f0:initWithFile(inputFile)
    return f0.width, f0.height
  end

  local W,H = unpacktuple(getWH())
  
  local observed = orion.image(rgbf,W,H)
  local latent_est = orion.image(rgbf,W,H)
  local convoutput = deconv(K,observed,latent_est,fusedIter)
  local deconvfn = orion.compile {convoutput}
  
  
  (terra()
     orion.bindConstantArray3Float32([latent_est:id()],W,H,array(.5f,.5f,.5f))
   end)()
  
  local observed_loaded = tofloat(orion.load(inputFile))
  
  orion.bindImage(observed:id(),observed_loaded())
  
  for i = 1, unfusedIter do
    orion.bindImage(latent_est:id(),deconvfn())
  end
  
  tobyte(latent_est):save("out/deconvlucy.bmp")
  
end