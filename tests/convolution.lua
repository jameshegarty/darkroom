terralib.require "test"
import "darkroom"

function convolution(N,K,in1)

  local im out(x,y)  0 end

  for i=-N,N do
    for j=-N,N do
      local ii = i+2
      local jj = j+2
      im out(x,y)  out(x,y) +  in1(x+i,y+j) / [K[ii][jj]] end
    end
  end

  return im(x,y) [uint8]( out(x,y) ) end
end


K = { {9, 9,9},
{9,9,9},
{9,9,9}}

test(convolution(1,K,testinput))