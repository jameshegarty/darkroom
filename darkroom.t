local cstdio = terralib.includec("stdio.h")
local cstdlib = terralib.includec("stdlib.h")


orion={}

terralib.require("common")
terralib.require("frontend")
terralib.require("types")
require "stencil"
require "optimizations"
require "ir"
terralib.require("ast")
require "typedAST"
require "kernelgraph"
require "schedule"
terralib.require("api")
terralib.require("backend_terra")

return {
  name = "darkroom";
  entrypoints = {"im"};
  keywords = {"map","reduce","let","in","switch","default","case"};
  statement = function(self,lex)
    local imfunc = orion.Parser.Parse(orion.lang,lex,"imageFunction")

    return 
      function(envfn)
      return orion.compileTimeProcess(imfunc,envfn)
      end, {imfunc.identifier}
  end;
  localstatement = function(self,lex)
    local imfunc = orion.Parser.Parse(orion.lang,lex,"imageFunction")
 
    return 
      function(envfn)
      return orion.compileTimeProcess(imfunc,envfn)
      end, {imfunc.identifier}
  end;
  expression = function(self,lex)
    local imfunc = orion.Parser.Parse(orion.lang,lex,"imageFunction")

    return 
      function(envfn)
      return orion.compileTimeProcess(imfunc,envfn)
      end
  end
}
