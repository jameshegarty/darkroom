import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")

function histogram(binCount, radiusX, radiusY, f)
  local bins = {}
  for b=0,binCount do
    table.insert(bins, im(x,y) map i=-radiusX,radiusX j=-radiusY, radiusY reduce(sum) [f(i,j,b)] end end)
  end
  return im(x,y) [bins] end
end

sensor = darkroomSimple.load("frame_128.bmp")
dx = im(x,y) (sensor(x+1,y)-sensor(x-1,y))/[uint8](2) end
dy = im(x,y) (sensor(x,y+1)-sensor(x,y-1))/[uint8](2) end
h = histogram(36,4,4,
              function(i,j,bin) 
                return im(x,y) 
                  deg = darkroom.arctan([float](dy(x+i,y+j))/[float](dx(x+i,y+j)))*[float](57.29)
                  degf = darkroom.floor(deg/10)
                  in (if degf==bin then [uint8](1) else [uint8](0) end) 
                end
              end)

orientation = im(x,y) [uint8]((map bin=0,36 reduce(argmin) h[bin] end)[0]) end

orientation:save("out/orientation.bmp")