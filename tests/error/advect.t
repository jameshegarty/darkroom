import "darkroom"
(terralib.loadfile("test.t"))()

maxv = 3
N = 64

function advect (d, d0, u, v, dt )
  assert(darkroom.ast.isAST(d))
  assert(darkroom.ast.isAST(d0))
  assert(darkroom.ast.isAST(u))
  assert(darkroom.ast.isAST(v))
  assert(type(dt)=="number")

  local im tx(x,y) [int32](-dt*u(x,y)*N) end
  local im ty(x,y) [int32](-dt*v(x,y)*N) end

  return im(x,y) darkroom.crop(darkroom.gather(d0,tx,ty,maxv,maxv)) end
end

darkroomSimple.setImageSize(64,64)
local im u(x,y) [float](0) end
local im v(x,y) [float](1) end
local d_delta = u
local d = im(x,y) darkroom.crop([float](0)) end
local d,d_delta = advect ( d, d_delta, u, v, 0.02 )
test({u,v,d})