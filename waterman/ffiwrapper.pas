unit ffiwrapper;

{$mode ObjFPC}{$H+}

{$linklib stdc++}// cpp link
{$linklib gcc_s}

{$linklib cpp/waterman.a}

interface

uses
  Classes, SysUtils, common;

// ffi wrap (nim/cpp)
procedure generateWaterman(radius: single; var nfaces, nVertexes, maxNFace: integer);
  cdecl; external;
procedure getWatermanMesh(oFaces: TPInt; oVertex: TPPoint3d); cdecl; external;
procedure releaseWatermanMesh; cdecl; external;

implementation

end.
