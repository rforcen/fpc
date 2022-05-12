# waterman poly / convex hull
# nim c -d:release -d:danger --app:staticlib --noMain:on --mm:arc waterman

import convexhull_ptr, wp, vec

var
    faces    : seq[seq[int]]
    vertices : seq[Point3D]
    
# c interface
{.push cdecl, exportc.}
proc generateWaterman*(radius:float32, nfaces:var int32,  nvertexes:var int32) =
    (faces, vertices) = convexHull waterman_poly(radius)
   
    var iface=0
    
    for face in faces: # count faces in line: <len><face ix>, <len><face ix1> ...
        iface+=face.len+1
        
    nfaces=iface.int32
    nvertexes = vertices.len.int32

proc getWatermanMesh(oLinedfaces:Vec[int32], oVertex:Vec[Point3D]) =
    var iface=0
    
    for i,v in vertices: oVertex[i]=v
    for face in faces: # faces in line: <len><face ix>, <len><face ix1> ...
        oLinedfaces[iface]=face.len.int32        
        for i,ix in face:
            oLinedfaces[iface+i+1]=ix.int32
        iface+=face.len+1
        
{.pop.}
