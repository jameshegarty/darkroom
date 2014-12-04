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

--XXX
dxf = im(x, y) darkroom.filter(x%64==0, [uint8] (dx)) end
dyf = im(x, y) darkroom.filter(x%64==0, [uint8] (dy)) end
dxf:save("out/dx.csv",{cores=1})
dyf:save("out/dy.csv",{cores=1})

-- Convert the gradient to polar coordinates
mag, angle = imgproc.cart2polar(dx, dy)

--XXX
magf = im(x, y) darkroom.filter(x%64==0, [uint8] (mag)) end
anglef = im(x, y) darkroom.filter(x%64==0, [uint8] (angle)) end
magf:save("out/mag.csv",{cores=1})
anglef:save("out/angle.csv",{cores=1})

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
maxbin = im(x,y) [uint8] ((map bin=0,nbins-1 reduce(argmax) h[bin] end)[0]) end

-- TODO: parabola interpolation on max bin

-- Filter operator allows for sparse input
maxbin = im(x,y) darkroom.filter(x%64==0,maxbin) end
maxbin:save("out/maxbin.csv",{cores=1})

-- Convert to fixed-point for display
--maxbin:save("out/maxbin.png",{cores=1})
