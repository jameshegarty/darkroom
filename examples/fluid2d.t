import "darkroom"
terralib.require "bilinear"
terralib.require "image"

cstdlib = terralib.includec("stdlib.h")
cstdio = terralib.includec("stdio.h")

N = 256
iter = 3
timestep = 0.1
maxv = 3

function add_source ( input_x, s, dt )
  assert(darkroom.ast.isAST(input_x))
  assert(darkroom.ast.isAST(s))
  assert(type(dt)=="number")

  local im add_source(x,y) input_x(x,y)+dt*s(x,y) end
  return add_source, s
end

function diffuse ( input_x, input_x0, diff, dt )
  assert(darkroom.ast.isAST(input_x))
  assert(darkroom.ast.isAST(input_x0))
  assert(type(diff)=="number")
  assert(type(dt)=="number")

  local a=dt*diff*N*N
  local b = 1/(1+4*a)

  for k=0,iter do
    im input_x(x,y) 
      (input_x0(x,y) + a*(input_x(x-1,y)+input_x(x+1,y)+input_x(x,y-1)+input_x(x,y+1)) )*b
    end
  end

  return input_x,input_x0
end

function advect (d, d0, u, v, dt )
  assert(darkroom.ast.isAST(d))
  assert(darkroom.ast.isAST(d0))
  assert(darkroom.ast.isAST(u))
  assert(darkroom.ast.isAST(v))
  assert(type(dt)=="number")

  local adv = resampleBilinear(
    true,
    d0,
    maxv,
    maxv,
    im(x,y) -dt*u(x,y)*N end,
    im(x,y) -dt*v(x,y)*N end)

  return im(x,y) adv(x,y) end, d0
end

function project ( u, v, p, div )
  assert(darkroom.ast.isAST(u))
  assert(darkroom.ast.isAST(v))
  assert(darkroom.ast.isAST(p))
  assert(darkroom.ast.isAST(div))

  local h = 1.0/N

  im div(x,y) -0.5*h*(u(x+1,y)-u(x-1,y)+v(x,y+1)-v(x,y-1)) end
  im p(x,y) 0 end

  local a = 1/4
  for k=0,iter do
    im p(x,y) (div(x,y)+p(x-1,y)+p(x+1,y)+p(x,y-1)+p(x,y+1))*a end
  end
  
  im u(x,y) u(x,y) - 0.5*(p(x+1,y)-p(x-1,y))*N end
  im v(x,y) v(x,y) - 0.5*(p(x,y+1)-p(x,y-1))*N end

  return u, v, p, div
end

function dens_step ( x, x0, u, v, diff, dt )

  assert(darkroom.ast.isAST(u))
  assert(darkroom.ast.isAST(v))

  assert(darkroom.ast.isAST(x))
  assert(darkroom.ast.isAST(x0))
  x,x0 = add_source ( x, x0, dt )
  x,x0 = x0,x
  assert(darkroom.ast.isAST(x))
  assert(darkroom.ast.isAST(x0))
  x,x0 = diffuse ( x, x0, diff, dt )
  x,x0 = x0,x
  assert(darkroom.ast.isAST(x))
  assert(darkroom.ast.isAST(x0))
  x,x0 = advect ( x, x0, u, v, dt )
  assert(darkroom.ast.isAST(x))
  assert(darkroom.ast.isAST(x0))
  
  return x,x0,u,v
end

function vel_step ( u, v, u0, v0, visc, dt )

  u,u0 = add_source ( u, u0, dt ) 
  v,v0 = add_source ( v, v0, dt )
  u,u0=u0,u
  v,v0=v0,v
  u,u0 = diffuse ( u, u0, visc, dt )
  v,v0 = diffuse ( v, v0, visc, dt )

  u,v,u0,v0 = project ( u, v, u0, v0 )
  u,u0=u0,u
  v,v0=v0,v
  u,u0 = advect ( u, u0, u0, v0, dt )
  v,v0 = advect ( v, v0, u0, v0, dt )
  u,v,u0,v0 = project ( u, v, u0, v0 )
  assert(darkroom.ast.isAST(u))
  assert(darkroom.ast.isAST(v))
  assert(darkroom.ast.isAST(u0))
  assert(darkroom.ast.isAST(v0))
  
  return u,v,u0,v0
end

function setupAndCompile()
  local u_in = darkroom.input( float )
  local v_in = darkroom.input( float )
  local d_in = darkroom.input( float )

  -- fluid source
  local im u_delta(x,y) [float](if x>11 and x<14 and y>11 and y<14 then 1 else 0 end) end
  local im v_delta(x,y) [float](0) end
  local im d_delta(x,y) [float](if x>11 and x<14 and y>11 and y<14 then 1 else 0 end) end

  local u,v,u_delta,v_delta = vel_step(u_in,v_in,u_delta,v_delta, 0, timestep)
  local d,d_delta,u,v = dens_step(d_in,d_delta,u,v,0,timestep)
  
  return darkroom.compile( {u_in,v_in,d_in}, {u,v,d}, {}, N, N, {printstage=true} )
end

calcFluid = setupAndCompile()

struct TapStruct {}

terra swap( a : &&opaque, b : &&opaque )
  var t = @a
  @a = @b
  @b = t
end

floatIm = darkroom.input(float)
im floatToUint8(x,y) [uint8](floatIm*255) end
floatToUint8 = darkroom.compile( {floatIm}, {floatToUint8}, {}, N, N )

terra run()
  var u_in = cstdlib.calloc( 1, N*N*sizeof(float) )
  var v_in = cstdlib.calloc( 1, N*N*sizeof(float) )
  var d_in = cstdlib.calloc( 1, N*N*sizeof(float) )

  var u_out = cstdlib.malloc( N*N*sizeof(float) )
  var v_out = cstdlib.malloc( N*N*sizeof(float) )
  var d_out = cstdlib.malloc( N*N*sizeof(float) )

  var tapStruct : TapStruct

  var iterations = 1
  for i=0,iterations do
    cstdio.printf("Do Iteration %d/50\n",i)
    calcFluid( u_in, v_in, d_in, u_out, v_out, d_out, &tapStruct )
    if i<iterations-1 then swap(&u_in, &u_out); swap(&v_in, &v_out); swap(&d_in, &d_out) end
  end
  
  var output : Image
  output:allocateDarkroomFormat(N,N,4,1,8,false,false)
  floatToUint8( d_out, output.data, &tapStruct )
  output:save("out/fluid2d.bmp")

  cstdlib.free(u_in); cstdlib.free(v_in); cstdlib.free(d_in)
  cstdlib.free(u_out); cstdlib.free(v_out); cstdlib.free(d_out)
end

run()