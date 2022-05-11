{OpenCTM write wrap: http://openctm.sourceforge.net/apidocs/ }

unit uctm;

{$mode ObjFPC}{$H+}
{$linklib openctm}

interface

uses
  Classes, SysUtils, uMesh, v3;

procedure writeMeshCTM(const mesh: TMesh; const Name: string);

implementation

type
  CTMcontext = pointer;
  CTMenum = (// Error codes (see ctmGetError())
    CTM_NONE = $0000, // No error has occured (everything is OK).
    //  Also used as an error return value for
    //  functions that should return a CTMenum
    //  value.
    CTM_INVALID_CONTEXT = $0001, // The OpenCTM context was invalid (e.g. NULL).
    CTM_INVALID_ARGUMENT = $0002, // A function argument was invalid.
    CTM_INVALID_OPERATION = $0003, // The operation is not allowed.
    CTM_INVALID_MESH = $0004, // The mesh was invalid (e.g. no pvertices).
    CTM_OUT_OF_MEMORY = $0005, // Not enough memory to proceed.
    CTM_FILE_ERROR = $0006, // File I/O error.
    CTM_BAD_FORMAT = $0007,
    // File format error (e.g. unrecognized format or corrupted file).
    CTM_LZMA_ERROR = $0008, // An error occured within the LZMA library.
    CTM_INTERNAL_ERROR = $0009, // An internal error occured (indicates a bug).
    CTM_UNSUPPORTED_FORMAT_VERSION = $000A, // Unsupported file format version.

    // OpenCTM context modes
    CTM_IMPORT = $0101,
    // The OpenCTM context will be used for importing data.
    CTM_EXPORT = $0102,
    // The OpenCTM context will be used for exporting data.

    // Compression methods
    CTM_METHOD_RAW = $0201, // Just store the raw data.
    CTM_METHOD_MG1 = $0202, // Lossless compression (floating point).
    CTM_METHOD_MG2 = $0203, // Lossless compression (fixed point).

    // Context queries
    CTM_VERTEX_COUNT = $0301, // Number of pvertices in the mesh (integer).
    CTM_TRIANGLE_COUNT = $0302, // Number of triangles in the mesh (integer).
    CTM_HAS_NORMALS = $0303, // CTM_TRUE if the mesh has normals (integer).
    CTM_UV_MAP_COUNT = $0304, // Number of UV coordinate sets (integer).
    CTM_ATTRIB_MAP_COUNT = $0305, // Number of custom attribute sets (integer).
    CTM_VERTEX_PRECISION = $0306, // Vertex precision - for MG2 (float).
    CTM_NORMAL_PRECISION = $0307, // Normal precision - for MG2 (float).
    CTM_COMPRESSION_METHOD = $0308, // Compression method (integer).
    CTM_FILE_COMMENT = $0309, // File comment (string).

    // UV/attribute map queries
    CTM_NAME = $0501, // Unique name (UV/attrib map string).
    CTM_FILE_NAME = $0502, // File name reference (UV map string).
    CTM_PRECISION = $0503, // Value precision (UV/attrib map float).

    // Array queries
    CTM_INDICES = $0601, // Triangle pindices (integer array).
    CTM_VERTICES = $0602, // Vertex point coordinates (float array).
    CTM_NORMALS = $0603, // Per vertex normals (float array).
    CTM_UV_MAP_1 = $0700, // Per vertex UV map 1 (float array).
    CTM_UV_MAP_2 = $0701, // Per vertex UV map 2 (float array).
    CTM_UV_MAP_3 = $0702, // Per vertex UV map 3 (float array).
    CTM_UV_MAP_4 = $0703, // Per vertex UV map 4 (float array).
    CTM_UV_MAP_5 = $0704, // Per vertex UV map 5 (float array).
    CTM_UV_MAP_6 = $0705, // Per vertex UV map 6 (float array).
    CTM_UV_MAP_7 = $0706, // Per vertex UV map 7 (float array).
    CTM_UV_MAP_8 = $0707, // Per vertex UV map 8 (float array).
    CTM_ATTRIB_MAP_1 = $0800, // Per vertex attribute map 1 (float array).
    CTM_ATTRIB_MAP_2 = $0801, // Per vertex attribute map 2 (float array).
    CTM_ATTRIB_MAP_3 = $0802, // Per vertex attribute map 3 (float array).
    CTM_ATTRIB_MAP_4 = $0803, // Per vertex attribute map 4 (float array).
    CTM_ATTRIB_MAP_5 = $0804, // Per vertex attribute map 5 (float array).
    CTM_ATTRIB_MAP_6 = $0805, // Per vertex attribute map 6 (float array).
    CTM_ATTRIB_MAP_7 = $0806, // Per vertex attribute map 7 (float array).
    CTM_ATTRIB_MAP_8 = $0807  // Per vertex attribute map 8 (float array).
    );

  pFloat = ^single;
  CTMuint = uint32;
  pCTMuint = ^CTMuint;
  PChar = ^char;

// api wrap (partial only to support writeCTM)
function ctmNewContext(mode: CTMenum): CTMcontext; cdecl; external;
procedure ctmCompressionMethod(context: CTMcontext; mode: CTMenum); cdecl; external;
procedure ctmDefineMesh(aContext: CTMcontext; aVertices: pFloat;
  aVertexCount: CTMuint; const aIndices: pCTMuint; aTriangleCount: CTMuint;
  const aNormals: pFloat); cdecl; external;
function ctmAddAttribMap(aContext: CTMcontext; const aAttribValues: pFloat;
  const aName: PChar): CTMenum; cdecl; external;
procedure ctmSave(context: CTMcontext; Name: PChar); cdecl; external;
function ctmGetError(context: CTMcontext): CTMenum; cdecl; external;
procedure ctmFreeContext(context: CTMcontext); cdecl; external;

procedure writeMeshCTM(const mesh: TMesh; const Name: string);
var
  context: CTMcontext;
  pos: array of vec3 = nil;
  norm: array of vec3 = nil;
  color: array of vec4 = nil;
  trigs: array of vec3u = nil;
  i, vertCount, triCount: int;
  trig: TTrig;
  map, errc: CTMenum;
begin
  context := ctmNewContext(CTM_EXPORT);

  vertCount := mesh.shape.Size;
  setLength(pos, vertCount);
  setLength(norm, vertCount);
  setLength(color, vertCount);

  for trig in mesh.trigs do // extract linear mesh
  begin
    for i in trig.tovec3u do
    begin
      pos[i] := mesh.shape[i].pos;
      norm[i] := mesh.shape[i].norm;
      color[i] := mkvec4(mesh.shape[i].color);
    end;
  end;

  triCount := mesh.trigs.Size;   // trigs
  setLength(trigs, triCount);
  for i := 0 to triCount - 1 do
    trigs[i] := mesh.trigs[i].tovec3u;

  ctmCompressionMethod(context, CTM_METHOD_MG2);
  ctmDefineMesh(context, @pos[0], vertCount, @trigs[0],
    triCount, @norm[0]);

  map := ctmAddAttribMap(context, @color[0], 'Color');
  if map = CTM_NONE then raise Exception.Create('CTM: error in ctmAttribMap:');

  ctmSave(context, @Name[1]);
  errc := ctmGetError(context);
  if errc <> CTM_NONE then
    raise Exception.Create(format('CTM: error saving file: %s0 code: %1d',
      [Name, errc]));

  ctmFreeContext(context);

end;

end.
