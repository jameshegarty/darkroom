import "orion"

-- test the perf model instead of actually runing the sim
perfModelTest = false
looptimes = 20

seed = 0
if perfModelTest then
  -- make the random scheduler do different stuff each runtime
  seed = math.floor(os.time())
  math.randomseed( seed )
  math.random(); math.random(); math.random()
end

terralib.require "common"

ffi = require "ffi"

N = 256
iter = 3
timestep = 0.1
maxv = 3

local C = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <inttypes.h>

double CurrentTimeInSecondsT() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + tv.tv_usec / 1000000.0;
}

]]

function add_source ( input_x, s, dt )

  assert(orion.ast.isAST(input_x))
  assert(orion.ast.isAST(s))
  assert(type(dt)=="number")
--  int i, size=(N+2)*(N+2);
--  for ( i=0 ; i<size ; i++ ) x[i] += dt*s[i]; 
  local im add_source(x,y) input_x(x,y)+dt*s(x,y) end
  return add_source, s
end

function diffuse ( input_x, input_x0, diff, dt )
  assert(orion.ast.isAST(input_x))
  assert(orion.ast.isAST(input_x0))
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

--[[
-- need to figure out how to get rid of the select here
function advect (d, d0, u, v, dt )
  assert(orion.ast.isAST(d))
  assert(orion.ast.isAST(d0))
  assert(orion.ast.isAST(u))
  assert(orion.ast.isAST(v))
  assert(type(dt)=="number")

  -- a technical limitation
  d0:materialize()
  u:materialize()
  v:materialize()

  return orion.call(
    terra(out:&float, dd:&float, uu:&float, vv:&float)
      for j = 0,N do
        for i = 0,N do
          
          var offx : float = dt*N*uu[j*width:as(int)+i];
          var offy : float = dt*N*vv[j*width:as(int)+i];
          var x : float= i-offx; 
          var y : float = j-offy;
          
          if(x<0 or x>=width-1 or y < 0 or y>=height-1) then
            out[j*width:as(int)+i] = 0
          else
            var i0:int=cmath.floor(x);
            var i1:int=i0+1; 
            var j0:int=cmath.floor(y);
            var j1:int=j0+1;
            
            var s1 = x-i0; 
            var s0 = 1-s1; 
            var t1 = y-j0; 
            var t0 = 1-t1;
            
            out[j*width:as(int)+i] = t0*(s0*dd[i0+j0*width:as(int)]+s1*dd[i1+j0*width:as(int)])+
              t1*(s0*dd[i0+j1*width:as(int)]+s1*dd[i1+j1*width:as(int)]);
          end
        end
      end
    end, orion.type.float(32), {d0, u, v}), d0
end
]]

function advect (d, d0, u, v, dt )
  assert(orion.ast.isAST(d))
  assert(orion.ast.isAST(d0))
  assert(orion.ast.isAST(u))
  assert(orion.ast.isAST(v))
  assert(type(dt)=="number")

  local adv = resampleBilinear(
    true,
    d0,
    maxv,
    maxv,
    im(x,y) -dt*u(x,y)*N end,
    im(x,y) -dt*v(x,y)*N end)

  -- by default, resample Bilinear has cropNone
  return im(x,y) adv(x,y) end, d0
end

function project ( u, v, p, div )
  assert(orion.ast.isAST(u))
  assert(orion.ast.isAST(v))
  assert(orion.ast.isAST(p))
  assert(orion.ast.isAST(div))

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

  assert(orion.ast.isAST(u))
  assert(orion.ast.isAST(v))

  assert(orion.ast.isAST(x))
  assert(orion.ast.isAST(x0))
  x,x0 = add_source ( x, x0, dt )
  x,x0 = x0,x
  assert(orion.ast.isAST(x))
  assert(orion.ast.isAST(x0))
  x,x0 = diffuse ( x, x0, diff, dt )
  x,x0 = x0,x
  assert(orion.ast.isAST(x))
  assert(orion.ast.isAST(x0))
  x,x0 = advect ( x, x0, u, v, dt )
  assert(orion.ast.isAST(x))
  assert(orion.ast.isAST(x0))
  
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
  assert(orion.ast.isAST(u))
  assert(orion.ast.isAST(v))
  assert(orion.ast.isAST(u0))
  assert(orion.ast.isAST(v0))
  
  return u,v,u0,v0
end

u_orig = {}
v_orig = {}
d_orig = {}

function setupAndCompile()
  local u = orion.constant( orion.type.float(32), N, N, 0 )
  u_orig = u
  local v = orion.constant( orion.type.float(32), N, N, 0 )
  v_orig = v
  local d = orion.constant( orion.type.float(32), N, N, 0 )
  d_orig = d

  local terra init()
    var u_delta_im_data = [&float](C.malloc(N*N*sizeof(float)))
    var v_delta_im_data = [&float](C.malloc(N*N*sizeof(float)))
    var d_delta_im_data = [&float](C.malloc(N*N*sizeof(float)))
  
    for i=0,N do
      for j=0,N do
        if i>11 and i<14 and j>11 and j<14 then 
          d_delta_im_data[N*j+i] = 1
          u_delta_im_data[N*j+i] = 1
          v_delta_im_data[N*j+i] = 0
        else
          d_delta_im_data[N*j+i] = 0
          u_delta_im_data[N*j+i] = 0
          v_delta_im_data[N*j+i] = 0
        end
      end
    end
    
    return u_delta_im_data, v_delta_im_data, d_delta_im_data
  end

  local u_delta_im_data, v_delta_im_data, d_delta_im_data = unpacktuple(init())

  local terra makeIm(data : &float, str : &int8)
    var img : Image
    img:initSimple(N,N,1,32,true,false,data)
    img:save(str)
    return img
  end

  local u_delta_im = makeIm(u_delta_im_data, "u_delta.bmp")
  local u_delta = orion.image(orion.type.float(32),N,N)
  orion.bindImage(u_delta:id(), u_delta_im)
  
  local v_delta_im = makeIm(v_delta_im_data, "v_delta.bmp")
  local v_delta = orion.image(orion.type.float(32),N,N)
  orion.bindImage(v_delta:id(), v_delta_im)

  local d_delta_im = makeIm(d_delta_im_data, "d_delta.bmp")
  local d_delta = orion.image(orion.type.float(32),N,N)
  orion.bindImage(d_delta:id(), d_delta_im)

  u,v,u_delta,v_delta = vel_step(u,v,u_delta,v_delta, 0, timestep)
  d,d_delta,u,v = dens_step(d,d_delta,u,v,0,timestep)
  
  local calcFluid, model = orion.compile(
    {u,v,d},
    {
--      schedule="random15", 
      calcPerfModel = false,
--     schedule="materialize", 
      verbose = false, debug = false, printruntime=(perfModelTest==false), looptimes=looptimes})

  return calcFluid, model
end

calcFluid, model = setupAndCompile()

if perfModelTest==false and false then
  for k,v in pairs(model.nodes) do
    print(k,v, model.compute[k], model.memory[k], model.latency[k])
  end
end

terra runIters()
  var uoutStr : int8[100]
  var voutStr : int8[100]
  var doutStr : int8[100]

  var iterations = 5
  if perfModelTest then iterations=1 end

  for i=0,iterations do

    var start = C.CurrentTimeInSecondsT()
    var uout,vout,dout = calcFluid()
    var endt = C.CurrentTimeInSecondsT()

--    C.printf("%d %f %f\n", seed, (endt-start)/looptimes, model.total)
    C.printf("%d %f\n", seed, (endt-start)/looptimes)

    C.snprintf(uoutStr,100,"out/u%d.bmp",i)
    uout:save(uoutStr)
    C.snprintf(voutStr,100,"out/v%d.bmp",i)
    vout:save(voutStr)
    C.snprintf(doutStr,100,"out/d%d.bmp",i)
    dout:save(doutStr)

    -- make a copy of the final image
    if i==iterations-1 then
      C.snprintf(doutStr,100,"out/fluid2d.bmp",i)
      dout:save(doutStr)
    end

    orion.bindImage( [u_orig:id()], &uout )
    orion.bindImage( [v_orig:id()], &vout )
    orion.bindImage( [d_orig:id()], &dout )
    uout:free()
    vout:free()
    dout:free()
  end
end

runIters()