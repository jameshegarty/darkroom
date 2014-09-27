(terralib.loadfile("test.t"))()
import "darkroom"

a = testinput
im b(x,y) 
[uint8](darkroom.crop(([uint16](a(x-1,y-1))+[uint16](a(x,y-1))+[uint16](a(x+1,y-1))+
                       [uint16](a(x-1,y))+[uint16](a(x,y))+[uint16](a(x+1,y))+
                       [uint16](a(x-1,y+1))+[uint16](a(x,y+1))+[uint16](a(x+1,y+1)))>>[uint8](3)))
end


im c(x,y) 
[uint8](darkroom.crop(([uint16](b(x-1,y))+[uint16](b(x,y))+[uint16](b(x+1,y)))>>[uint8](1)))
end


im d(x,y) 
[uint8](darkroom.crop(([uint16](c(x-1,y-1))+[uint16](c(x,y-1))+[uint16](c(x+1,y-1))+
                       [uint16](c(x-1,y))+[uint16](c(x,y))+[uint16](c(x+1,y))+
                       [uint16](c(x-1,y+1))+[uint16](c(x,y+1))+[uint16](c(x+1,y+1)) )>>[uint8](3)))
end



test(d)