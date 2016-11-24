import "darkroom"
darkroomSimple = require("darkroomSimple")

fusedIter = 5

function tofloat(input) return im(x,y) [float[3]](input(x,y)/([float](255))) end end

function tobyte(input)
  return im(x,y) darkroom.vectorSelect( input>1, 
                 [uint8[3]](255), [uint8[3]](input*255) ) end
end

function convolve( K, input )
    local N2 = math.floor(math.sqrt(#K))
    assert(N2 % 2 == 1 and N2*N2 == #K)
    local N = math.floor(N2/2)

    return im(x,y)
      darkroom.crop( map i=-N,N j=-N,N reduce(sum)
                       input(x+i, y+j) * K[(-j + N)*N2 + (i + N)]
                     end)
    end
end

local function deconv( K, Khat, observed, latent_est, N)
  for i = 1,N do
    local est_conv = convolve(K,latent_est)
    local relative_blur = im(x,y) darkroom.crop(observed(x,y) / est_conv(x,y)) end
    local error_est = convolve(Khat,relative_blur)
    latent_est = im(x,y) darkroom.crop(latent_est(x,y)*error_est(x,y)) end
  end
  return tobyte(latent_est)
end

local K = {0.33333,0,0,
           0,0.33333,0,
           0,0,0.33333}

local observed = tofloat(darkroomSimple.load("deconvolution_blurred.bmp"))
local latent_est = im(x,y) [float[3]](0.5) end -- initial value
local convoutput = deconv( K, K, observed, latent_est, fusedIter )
convoutput:save("out/deconv-simple.bmp")
