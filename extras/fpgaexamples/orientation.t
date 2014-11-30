import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
imgproc = terralib.require("imgproc")

nbins = 8 -- Number of orientation bins
hw = 8 -- Half-width of the orientation window

function histogram(binCount, radiusX, radiusY, f)
  return im(x,y) map b = 0, binCount - 1 reduce(none) map i = -radiusX, radiusX j = -radiusY, radiusY reduce(sum) [f(i,j,b)] end end end
end

sensor = darkroomSimple.load("frame_128.bmp")

-- Get the image gradient 
-- TODO: blur the data first, make this an optional parameter
dx, dy = imgproc.gradient(sensor)

-- Convert the gradient to polar coordinates
mag, angle = imgproc.cart2polar(dx, dy)

h = histogram(nbins, hw, hw, 
              function(i,j,bin) 
                return im(x,y) 
		  -- Get the bin in integer coordinates
                  fbin = [float] (angle(x, y) * nbins / (2 * math.pi))
		  ibin = [uint8] (darkroom.floor(fbin))

		  -- Get the remainder
		  rbin = [float] (fbin - [float] (ibin))

		  -- Use linear interpolation to accumulate the gradient into 
		  -- this bin
                  in (if ibin == bin then [float] (mag * (1 - rbin)) else (if ibin == bin + 1 then [float] (mag * rbin) else [float] (0) end) end)
                end
              end)

-- use argmax to find the bin with the largest value
orientation = im(x,y) [float] ((map bin=0,nbins-1 reduce(argmax) h[bin] end)[0]) end

-- TODO: parabola interpolation on max bin

-- Filter operator allows for sparse input
orientation = im(x,y) darkroom.filter(x%64==0,orientation) end
orientation:save("out/orientation.csv",{cores=1})
