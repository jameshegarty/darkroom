terralib.require "test"
import "darkroom"

--a=testinput
im a(x,y)   [float](testinput(x,y)) end

im b(x,y) 
  darkroom.crop((a(x-1,y-1)+a(x,y-1)+a(x+1,y-1)+
              a(x-1,y)+a(x,y)+a(x+1,y)+
              a(x-1,y+1)+a(x,y+1)+a(x+1,y+1))/9)
end


im c(x,y) 
darkroom.crop((b(x-1,y)+b(x,y)+b(x+1,y))/3)
end


im d(x,y) 
  darkroom.crop((c(x-1,y-1)+c(x,y-1)+c(x+1,y-1)+
              c(x-1,y)+c(x,y)+c(x+1,y)+
              c(x-1,y+1)+c(x,y+1)+c(x+1,y+1) )/9)
end



test(im(x,y) [uint8](d(x,y)) end)