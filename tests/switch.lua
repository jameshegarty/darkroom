terralib.require("test")
import "darkroom"

test(im(x,y) : uint8
     switch x%4
       case 0 -> testinput(x,y)
       case 1 -> testinput(x,y)+10
       case 2 -> testinput(x,y)+20
       default -> testinput+30
       end
end)
