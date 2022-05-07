#[
    fpc/c/c++ algSurf interface
    
 generate --app:staticlib static lib .a
 remove main symbol --noMain:on,
 arc cg --mm:arc, mem freed when out of scope
 
 nim c -d:release -d:danger --app:staticlib --noMain:on --mm:arc engine.nim 
]#

import vec, AlgSurf
   
# interface algSurf
{.push cdecl, exportc.}

func getNVertexes*(resol:cint) : cint =
    (resol+1)*(resol+1)
    
proc algSurf*(nas, resol : cint, vertexes : Vec[QuadNormal]) =
    let vtx = eval(nas, resol)
    doAssert vtx.len.cint == getNVertexes(resol), "len of quadnormal vertexes and reserved size don't match"
    for i, v in vtx:
        vertexes[i]=v   
    
func getQuadNormalSize* :cint=
    QuadNormal.sizeof.cint
##

{.pop.}
