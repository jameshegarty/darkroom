import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")

fusedIter = 5
unfusedIter = 10

function tofloat(input)
    assert(input)
    return im(x,y) [float[3]](input(x,y)/([float](255))) end
end

function invert(input) return im(x,y) 1.0 - input(x,y) end end

function tobyte(input)
    assert(input)
    return im(x,y)
        t = input(x,y)*(255) 
        in [uint8[3]]({if t[0] > 255.0 then 255.0 else t[0] end,
                       if t[1] > 255.0 then 255.0 else t[1]end,
                       if t[2]  > 255.0 then 255.0 else t[2] end})
    end

end

function convolve(K,input)
    local N2 = math.floor(math.sqrt(#K))
    assert(N2 % 2 == 1 and N2*N2 == #K)
    local N = math.floor(N2/2)

    return im(x,y)
      res = map i=-N,N j=-N,N reduce(sum)
        idx =  (-j + N)*N2 + (i + N)
        in input(x+i, y+j) * K[idx]
      end
      in darkroom.crop(res)
    end
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
        local relative_blur = im(x,y) darkroom.crop(observed(x,y) / est_conv(x,y)) end
        local error_est = convolve(Khat,relative_blur)
        latent_est = im(x,y) darkroom.crop(latent_est(x,y)*error_est(x,y)) end
    end
    return latent_est
end


local K = normalizekernel {1,0,0,
                           0,1,0,
                           0,0,1}

function generateBlurredInput()
  local input = darkroomSimple.load("color.bmp")                           
  tobyte(convolve(K,tofloat(input))):save("out/deconvolution_blurred.bmp")
end
generateBlurredInput()

struct TapStruct {}
function doDeconvolution()
  local inputFile = arg[1] or "out/deconvolution_blurred.bmp"

  -- work around calling convention limitations on ARM
  local terra getW() var f0 : Image; f0:load(inputFile); var w = f0.width; f0:free(); return w end
  local terra getH() var f0 : Image; f0:load(inputFile); var h = f0.height; f0:free(); return h end

  local W = getW()
  local H = getH()

  local observed = darkroom.input(float[3])
  local latent_est = darkroom.input(float[3])
  local convoutput = deconv(K,observed,latent_est,fusedIter)
  local deconvfn = darkroom.compile( {observed, latent_est}, {convoutput}, {}, W, H )
  
  local byteimg = darkroom.input(uint8[3])
  local tofloatfn = darkroom.compile( {byteimg}, {tofloat(byteimg)}, {}, W, H )

  local floatimg = darkroom.input(float[3])
  local tobytefn = darkroom.compile( {floatimg}, {tobyte(floatimg)}, {}, W, H )
  
  (terra()
     var tapStruct : TapStruct

     -- load the input
     var observed : Image
     observed:load(inputFile):toDarkroomFormat()

     var observedFloat : Image
     observedFloat:allocateDarkroomFormat(W,H,4,3,32,true,false)

     tofloatfn(observed.data, observedFloat.data, &tapStruct)
  
     observed:free() -- no longer needed once we convert to float

     -- allocate the output
     var latent_est : Image
     latent_est:allocateDarkroomFormat(W,H,4,3,32,true,false)
     var latent_est_ptr = latent_est.data
     for c=0,3 do for y=0,H do for x=0,W do
           ([&float](latent_est.data))[c*W*H+y*W+x] = 0.5
     end end end

     var out : Image
     out:allocateDarkroomFormat(W,H,4,3,32,true,false)
     var out_ptr = out.data

     -- run the pipeline multiple times
     for i = 1, unfusedIter+1 do
       deconvfn( observedFloat.data, latent_est_ptr, out_ptr, &tapStruct )

       if i<unfusedIter then
         var t = latent_est_ptr
         latent_est_ptr = out_ptr
         out_ptr = t
       end
     end
  
     -- save out
     var result : Image
     result:allocateDarkroomFormat(W,H,4,3,8,false,false)
     tobytefn( out_ptr, result.data, &tapStruct )
     result:fromDarkroomFormat():save("out/deconvolution.bmp")
   end)()  
end
doDeconvolution()