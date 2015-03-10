local ffi = require("ffi")

d = {}

dFunctionFunctions = {}
dFunctionMT={__index=dFunctionFunctions}

dASTFunctions = {}
setmetatable( dASTFunctions,{__index=IRFunctions})
dASTMT = {__index = dASTFunctions }

function newDAST(tab)
  assert( type(tab) == "table" )
  darkroom.IR.new( tab )
  return setmetatable( tab, dASTMT )
end

function isDFn(t) return getmetatable(t)==dFunctionMT end
function isDAST(t) return getmetatable(t)==dASTMT end

-- arrays: A[W][H]. Row major
-- array index A[y] yields type A[W]. A[y][x] yields type A

local function outermostDimension(ty)
  assert(darkroom.type.isType(ty))
  assert( ty:isArray() )
  local sz = ty:arrayLength()
  return sz[#sz]
end

local function peel( ty )
  assert(darkroom.type.isType(ty))
  assert( ty:isArray() )
  if #ty:arrayLength()==1 then return ty:baseType() end
  return darkroom.type.array( ty:baseType(), take(ty:arrayLength(), #(ty:arrayLength())-1) )
end

-- f : ( A, B, ...) -> C (darkroom function)
-- map : ( f, A[n], B[n], ...) -> C[n]
function d.map( f, ... )
  assert( isDFn(f) )
  for k,v in pairs({...}) do 
    assert( isDAST(v) ); 
    if v.type:isArray()==false or peel(v.type) ~= f.inputs[k].type then print("Error, map type mismatch, formal=",f.inputs[k].type," actual=",v.type,"argument",k); assert(false); end
  end
  assert( foldl( function(a,b) return sel(a==true or a==b, b, nil) end, true, map( {...}, function(n) return outermostDimension(n.type) end ) ) )
  local ast = {kind="map", fn = f, type = darkroom.type.array( f.output.type, {outermostDimension(({...})[1].type)} ) }
  for k,v in pairs({...}) do ast["input"..k] = v end
  return newDAST(ast):setEmptyMetadata()
end

-- dup : ( v : A , n : number ) -> A[n]
function d.dup( v, n )
  assert(isDAST(v))
  assert(type(n)=="number")
  return newDAST({kind="dup", expr=v, type = darkroom.type.array(v.type,{n})}):setEmptyMetadata()
end

-- extractStencils : A[n] -> A[stencilWidth*stencilHeight][n]
function d.extractStencils( inp, arrayWidth, stencilWidth, stencilHeight )
  assert( isDAST(inp) )
  assert( type(arrayWidth)=="number" )
  assert( type(stencilWidth)=="number" )
  assert( stencilWidth<0 )
  assert( type(stencilHeight)=="number" )
  assert( stencilHeight<0 )

  local ty = darkroom.type.array(peel(inp.type),{(-stencilHeight+1)*(-stencilWidth+1), outermostDimension(inp.type)})
  return newDAST({kind="extractStencils", expr=inp, arrayWidth=arrayWidth, stencilWidth=stencilWidth, stencilHeight=stencilHeight, type = ty}):setEmptyMetadata()
end

-- slice : A[n] -> A[ (r-l+1) * (t-b+1) ]
-- l,r,t,b are inclusive
function d.slice( A, arrayWidth, l, r, t, b )
  assert(l<=r)
  assert(b<=r)
  print("SLICE",l,r,t,b)
  assert( type(arrayWidth)=="number" )
  print("SLICETYPE",A.type,peel(A.type),(r-l+1)*(t-b+1))
  local ty = darkroom.type.array( peel(A.type), {(r-l+1)*(t-b+1)} )
  print("STR",ty)
  return newDAST({kind="slice", expr=A, l=l,r=r,t=t,b=b, type = ty } ):setEmptyMetadata()
end

-- reduce : A[n] -> A
-- this is just a special case of function application
function d.reduce( op, A )
  assert( type(op)=="string" )
  return newDAST({kind="reduce", op=op, expr=A, type = peel(A.type)} ):setEmptyMetadata()
end

-- function argument
function d.input( type )
  return newDAST( {kind="input", type = type} ):setEmptyMetadata()
end

-- function definition
-- output, inputs
function d.fn( output, ... )
  assert( isDAST(output) )
  map( {...}, function(n) assert(isDAST(n)); assert(n.kind=="input") end )
  local t = {output=output, inputs = {...} }
  return setmetatable( t, dFunctionMT )
end

function d.leaf( fn, outputType, ... )
  assert( darkroom.type.isType(outputType) )
  map( {...}, function(n) assert(isDAST(n)) end )
  return setmetatable( {kind="terra", fn=fn, output={type=outputType},inputs={...} }, dFunctionMT )
end

function d.range( v, ty )
  assert(type(v)=="number")
  assert( darkroom.type.isType(ty) )
  return newDAST( {kind="range", value=v, type=darkroom.type.array(ty,{v})} ):setEmptyMetadata()
end

function d.apply( fn, ... )
  assert(isDFn(fn))
  for k,v in ipairs({...}) do if v.type~=fn.inputs[k].type then print("Apply type error. formal=",fn.inputs[k].type,"actual=",v.type); assert(false) end end
  local t = {kind="apply", fn=fn, type = fn.output.type}
  for k,v in ipairs({...}) do t["input"..k] = v end
  return newDAST( t ):setEmptyMetadata()
end

local __compileCache = {}
function d.compile( fn )
  assert(isDFn(fn))

  if __compileCache[fn]==nil then
    if fn.kind=="terra" then
      __compileCache[fn] = fn.fn
    else
      assert( isDAST(fn.output) )
      __compileCache[fn] = fn.output:visitEach(
        function(n, inputs)
          if n.kind=="map" then
            local orderedInputs = n:map( "input", function(_,i) return inputs["input"..i] end )
            local f = d.compile(n.fn)
            local N = outermostDimension( n.type )
            return function(...)
              local args = {...}
              local inpv = map( orderedInputs, function(f) return f(unpack(args)) end)
              local out = {}
              for i=0,N-1 do 
                local inp = map( inpv, function(v) return v[i] end )
                out[i] = f(unpack(inp))
              end
              return ffi.new(n.type:toC(),out)
                   end
          elseif n.kind=="input" then
            local inpinv = invertTable( fn.inputs )
            local i = inpinv[n]
            assert(type(i)=="number")
            return function(...) return ({...})[i] end
          elseif n.kind=="extractStencils" then
            local ef = inputs.expr
            local N = outermostDimension( n.type )
            local stencilW = -n.stencilWidth+1
            return function(...)
              local inp = ef(...)
              local out = {}
              for i=0,N do
                out[i]={}
                for y=n.stencilHeight,0 do
                  for x=n.stencilWidth,0 do
                    out[i][(y-n.stencilHeight)*stencilW+(x-n.stencilWidth)] = inp[i+x+y*n.arrayWidth]
                  end
                end
                out[i] = ffi.new( peel(n.type):toC(), out[i])
              end
              return out
                   end
          elseif n.kind=="range" then
            return function(...)
              local out = {}
              for i=1,n.value do
                out[i-1] = i
              end
              return ffi.new( n.type:toC(), out )
                   end
          elseif n.kind=="reduce" then
            local ef = inputs.expr

            if n.op=="sum" then
              return function(...)
                local inp = ef(...)
                local res = 0
                for i=0,(n.expr.type:arrayLength())[1]-1 do res = res + inp[i] end
                return ffi.new( n.type:toC(), res )
                     end
            else
              assert(false)
            end
          elseif n.kind=="apply" then
            local f = d.compile( n.fn )
            local orderedInputs = n:map( "input", function(_,i) return inputs["input"..i] end )
            return function(...)
              local args = {...}
              local inpv = map( orderedInputs, function(f) return f(unpack(args)) end)
              return f(unpack(inpv))
                   end
          elseif n.kind=="slice" then
          else
            print(n.kind)
            assert(false)
          end
        end)
    end
  end

  return __compileCache[fn]
end
------------------------

-- extracts stencils of size (stencilWidth x stencilHeight) from 0 ... down to -stencilCount
function d.extractStencilArray( A, arrayWidth, stencilWidth, stencilHeight, stencilCount)
  assert(type(arrayWidth)=="number")
  assert(type(stencilWidth)=="number")
  assert(type(stencilHeight)=="number")
  assert(type(stencilCount)=="number")
  assert(stencilCount>0)
  local largeStencilWidth = stencilWidth-stencilCount+1
  local s1 = d.extractStencils( A, arrayWidth, largeStencilWidth, stencilHeight )
  
  ------------
  local subinput = d.input( peel(s1.type) )
  local stencils = d.extractStencils( subinput, largeStencilWidth, stencilWidth, stencilHeight )
  local slice = d.slice( stencils, largeStencilWidth, -stencilCount+1, 0, stencilHeight-1, stencilHeight-1)
  local substencils = d.fn( slice, subinput )
  -----------

  local fin = d.map( substencils, s1 )
  return fin
end