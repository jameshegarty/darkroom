stdlib = terralib.includec("stdlib.h")
cstdio = terralib.includec("stdio.h")
cpthread = terralib.includec("pthread.h")
terralib.require("vector")

orion.runtime = {}
orion.runtime.verbose = false
orion.runtime.mutex = global(cpthread.pthread_mutex_t)

orion.runtime.registerSize = global(int) -- size of the registers in bytes
orion.runtime.inactiveRegisters = global(Vector(&opaque)) -- registers that can be reused
-- data is array that holds the data for this register
-- data can be nil, if this register wasn't actually allocated yet (or is freed)
-- (we're using a vector as a map so it may allocate some registerStates that aren't needed)
struct RegisterState {data : &opaque, refCount : int}
orion.runtime.registerMap = global(Vector(RegisterState)) -- maps a register ID to a register in the registerTable

terra orion.runtime.init()
  if orion.runtime.verbose then cstdio.printf("runtime init\n") end

  orion.runtime.registerSize = 0
  orion.runtime.inactiveRegisters:init()
  orion.runtime.registerMap:init()

  cpthread.pthread_mutex_init(&orion.runtime.mutex,nil)
end

orion.runtime.init()

-- if you call this multiple times, it'll be set to the largest
terra orion.runtime.setRegisterSize(sizeInBytes : int)
  if orion.runtime.verbose then cstdio.printf("setRegisterSize %d\n",sizeInBytes) end

  if sizeInBytes > orion.runtime.registerSize then
    --cstdio.printf("Active Registers %d\n",orion.runtime.registerMap:count())
    if orion.runtime.verbose then cstdio.printf("need to cleanup existing registers\n") end

    var activeRegCount = 0
    for i=0,orion.runtime.registerMap:count() do
      if orion.runtime.registerMap:get(i).data~=nil then activeRegCount= activeRegCount+1 end
    end

--    orionAssert( activeRegCount == 0, "changing size while there's active registers!")  JB commented out for debug
--    orionAssert(orion.runtime.inactiveRegisters:count()==0, "need 2 impl this")
    while orion.runtime.inactiveRegisters:count()>0 do
      var theReg = orion.runtime.inactiveRegisters:pop()
      stdlib.free(theReg)
      if orion.runtime.verbose then cstdio.printf("free inactive register that's too small\n") end
    end

    orion.runtime.registerSize = sizeInBytes
  end
  if orion.runtime.verbose then cstdio.printf("register size set to %d\n",orion.runtime.registerSize) end
end

-- registers are identified by an integer ID
-- different images should have a different ID (obviously)
terra orion.runtime.createRegister(id:int, refCount:int)
  cpthread.pthread_mutex_lock(&orion.runtime.mutex)

  if orion.runtime.verbose then cstdio.printf("create register %d\n",id) end

  if id < orion.runtime.registerMap:count() then
    var registerInfo = orion.runtime.registerMap:get(id)

    if registerInfo.data~=nil then
      orionAssert(false, "Register already exists!")
    end
  end

  orionAssert(refCount>0, "creating a register with a refCount <= 0 is pointless!")

  var theReg : &opaque
  if orion.runtime.inactiveRegisters:count()>0 then
    -- are there any free registers we can just use?
    if orion.runtime.verbose then cstdio.printf("reusing an old register\n") end
    theReg = orion.runtime.inactiveRegisters:pop()
  else
    -- we need to alloc a new one
    orionAssert(orion.runtime.registerSize>0, "reg size not set\n")

    stdlib.posix_memalign( [&&opaque](&theReg), orion.tune.pageSize, orion.runtime.registerSize )

    --theReg = cstdlib.malloc(orion.runtime.registerSize)
    if orion.runtime.verbose then cstdio.printf("alloc new reg %d\n",theReg) end
  end

  orion.runtime.registerMap:set(id,
                                RegisterState {data = theReg,
                                 refCount=refCount}, RegisterState {data=nil,refCount=0})

  cpthread.pthread_mutex_unlock(&orion.runtime.mutex)

  return theReg
end

-- get a register that should already exist
terra orion.runtime.getRegister(id:int) : &opaque
  cpthread.pthread_mutex_lock(&orion.runtime.mutex)

  orionAssert(id < orion.runtime.registerMap:count(), "register doesn't exist")
  var registerInfo = orion.runtime.registerMap:get(id)

  if registerInfo.data==nil then
    cstdio.printf("getRegister %d\n",id)
  end

  orionAssert(registerInfo.data~=nil,"register is invalid")
  var res = registerInfo.data

  if orion.runtime.verbose then
    --cstdio.printf("get register %d %d\n",id,res)
    --cstdio.printf("active reg %d\n",registerInfo.activeRegister)
    --cstdio.printf("refcount %d\n",registerInfo.refCount)
  end
  
  cpthread.pthread_mutex_unlock(&orion.runtime.mutex)

  return res
end

-- registers are reference counted.
-- release ONE reference to this register. 
terra orion.runtime.releaseRegister(id:int)
  cpthread.pthread_mutex_lock(&orion.runtime.mutex)

  if orion.runtime.verbose then cstdio.printf("Release REgister %d\n",id) end

  orionAssert(id < orion.runtime.registerMap:count(), "releasing a nonexistant reg" )

  orionAssert(orion.runtime.registerMap:getPtr(id).refCount>0, "register already freed")

  orion.runtime.registerMap:getPtr(id).refCount = orion.runtime.registerMap:get(id).refCount-1
  if orion.runtime.registerMap:get(id).refCount == 0 then
    if orion.runtime.verbose then cstdio.printf("Register no longer needed... adding to inactive\n") end
    var theReg = orion.runtime.registerMap:get(id).data
    orion.runtime.inactiveRegisters:push(theReg)
    orion.runtime.registerMap:getPtr(id).data = nil
  end

  cpthread.pthread_mutex_unlock(&orion.runtime.mutex)
end

-- this releases the register, but doesn't reclaim the data storage area.
-- This is used to get data out of the pipe at the end.
-- this assumes the refCount is 1 and the register is ready to be released
terra orion.runtime.releaseAndKeep(id:int)
  cpthread.pthread_mutex_lock(&orion.runtime.mutex)

  if orion.runtime.verbose then cstdio.printf("Release And Keep %d\n",id) end

  orionAssert(id < orion.runtime.registerMap:count(), "releasing a nonexistant reg" )
  orion.runtime.registerMap:getPtr(id).refCount = orion.runtime.registerMap:get(id).refCount-1

  if orion.runtime.registerMap:get(id).refCount ~= 0 then
    cstdio.printf("refcount %d\n",orion.runtime.registerMap:get(id).refCount)
  end

  orionAssert(orion.runtime.registerMap:get(id).data ~= nil, "releaseAndKeep data is nil")
  orionAssert(orion.runtime.registerMap:get(id).refCount == 0, "releaseAndKeep refCount not 0")

  -- mark this register as dead
  orion.runtime.registerMap:getPtr(id).data = nil

  cpthread.pthread_mutex_unlock(&orion.runtime.mutex)
end

terra orion.runtime.activeRegisters()
  cpthread.pthread_mutex_lock(&orion.runtime.mutex)

  var activeReg = 0

  for id=0,orion.runtime.registerMap:count() do
    if orion.runtime.registerMap:get(id).refCount~=0 or
      orion.runtime.registerMap:get(id).data~=nil then
      cstdio.printf("Active Reg: %d refcount:%d\n",id, orion.runtime.registerMap:get(id).refCount)
      activeReg = activeReg + 1
    end
  end

  cpthread.pthread_mutex_unlock(&orion.runtime.mutex)

  return activeReg
end

----------------
orion.runtime.taps = global(Vector(&opaque))

terra orion.runtime.setTap(id : int, valueptr : &opaque)
  if id < orion.runtime.taps:count() and
    orion.runtime.taps:get(id)~=nil then
    cstdlib.free(orion.runtime.taps:get(id))
  end

  orion.runtime.taps:set(id,valueptr,nil)
end

terra orion.runtime.getTap(id : int) : &opaque
  orionAssert(id < orion.runtime.taps:count(), "tap not yet set!")
  return orion.runtime.taps:get(id)
end

----------------
orion.runtime.tapLUTs = global(Vector(&opaque))

terra orion.runtime.setTapLUT(id : int, valueptr : &opaque)
  if id < orion.runtime.tapLUTs:count() and
    orion.runtime.tapLUTs:get(id)~=nil then
    cstdlib.free(orion.runtime.tapLUTs:get(id))
  end

  orion.runtime.tapLUTs:set(id,valueptr,nil)
end

terra orion.runtime.getTapLUT(id : int) : &opaque
  orionAssert(id < orion.runtime.tapLUTs:count(), "tapLUT not yet set!")
  return orion.runtime.tapLUTs:get(id)
end