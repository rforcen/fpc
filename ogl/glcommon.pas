unit glCommon;

{$mode ObjFPC}{$H+}
{$modeswitch  advancedrecords}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Controls, Graphics, gl, OpenGLContext, Math;

type
  { basic geo types }
  realNumber = single;

  TFaceIndex = uint32;
  TFace = array of TFaceIndex;
  TFaces = array of TFace;
  TVertex = array [0..2] of realNumber;
  TTextureCoord = array[0..1] of realNumber;
  TTextureCoords = array of TTextureCoord;
  TVertexes = array of TVertex;
  TEdge = array[0..1] of TFaceIndex;
  TEdges = array of TEdge;
  TQuad = array [0..3] of TVertex;
  vec4f = array[0..3] of single;
  vec1f = array[0..0] of single;

  { TPolyhedron }

  TPolyhedron = record
    Name: string;
    faces: TFaces;
    vertexes, normals: TVertexes;
    textures: TTextureCoords;

    procedure calcNormals;
    procedure calcTextures;
  end;

  TPolyhedrons = array of TPolyhedron;
  TPolyType = (Tetrahedron, Cube, Icosahedron, Octahedron, Dodecahedron);


  { TTexture }

  TTexture = record
  public
    tID: integer;
    fileName: string;
  public
    function load(_fileName: string): boolean; overload;
    function load(pic: TPicture): boolean; overload;
    procedure bind;
    procedure enable;
    procedure disable;
    procedure Delete;
  end;

  TTextures = array of TTexture;

  { TGL }

  { usage:
      TradGL = class(TGL)
      public
        destructor Destroy; override;
        procedure draw; override;
        procedure init; override;
      private
        txsPeople, txsMandalas: TTextures;
      end;
  }

  TGL = class(TOpenGLControl)
  public
    constructor Create(_parent: TWinControl; _withLists: boolean = True);
    destructor Destroy; override;

    procedure init; virtual;
    procedure draw; virtual;
  public
    procedure identity; overload;
    procedure identity(ax, ay: realNumber); overload;
    procedure incAngles(ax, ay: realNumber);

    procedure useLists;
    procedure newList;

  private

    procedure glPaint(Sender: TObject);
    procedure glInit;
    procedure glResize(Sender: TObject);
    procedure glMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure glMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure glMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure glMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);


    procedure startList;
    procedure endList;
    procedure useList;


  private
    firstTime: boolean;
    _msDown: boolean;
    dispList: integer;
    withLists: boolean;
  public
    anglex, angley, offx, offy, scale: realNumber;
    lastx, lasty: integer;
  end;

  { TTube }
  TTube = class
  public
    // _track are from, to vertex pairs
    constructor Create(_track: TVertexes; _rad: realNumber; slices: integer;
      _color: TVertex);
    procedure draw;
  private
    _tubes: TPolyhedrons;
    color: TVertex;
    track: TVertexes;
    rad: realNumber;
  private
    function doTube(_from, _to: TVertex; r: realNumber; slices: integer): TPolyhedron;
  public
    property tubes: TPolyhedrons read _tubes;
  end;

const
  Polyhedrons: TPolyhedrons = (
    (Name: 'Tetrahedron'; faces: ((0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2));
    vertexes: ((1., 1, 1), (1, -1, -1), (-1, 1, -1), (-1, -1, 1))),
    (Name: 'Cube'; faces: ((3, 0, 1, 2), (3, 4, 5, 0), (0, 5, 6, 1),
    (1, 6, 7, 2), (2, 7, 4, 3), (5, 4, 7, 6));
    vertexes: ((1, 1, 1), (-1, 1, 1), (-1, -1, 1), (1, -1, 1),
    (1, -1, -1), (1, 1, -1), (-1, 1, -1), (-1, -1, -1))),
    (Name: 'Icosahedron'; faces: ((0, 1, 2), (0, 2, 3), (0, 3, 4), (0, 4, 5),
    (0, 5, 1), (1, 5, 7), (1, 7, 6), (1, 6, 2), (2, 6, 8), (2, 8, 3),
    (3, 8, 9), (3, 9, 4),
    (4, 9, 10), (4, 10, 5), (5, 10, 7), (6, 7, 11), (6, 11, 8),
    (7, 10, 11), (8, 11, 9), (9, 11, 10));
    vertexes: ((0, 0, 1.176), (1.051, 0, 0.526), (0.324, 1, 0.525), (-0.851, 0.618,
    0.526), (-0.851, -0.618, 0.526), (0.325, -1, 0.526),
    (0.851, 0.618, -0.526), (0.851, -0.618, -0.526), (-0.325, 1, -0.526),
    (-1.051, 0, -0.526), (-0.325, -1, -0.526), (0, 0, -1.176))),
    (Name: 'Octahedron'; faces: ((0, 1, 2), (0, 2, 3), (0, 3, 4), (0, 4, 1),
    (1, 4, 5), (1, 5, 2), (2, 5, 3), (3, 5, 4));
    vertexes: ((0, 0, 1.414), (1.414, 0, 0), (0, 1.414, 0), (-1.414, 0, 0),
    (0, -1.414, 0), (0, 0, -1.414))),
    (Name: 'Dodecahedron'; faces: ((0, 1, 4, 7, 2), (0, 2, 6, 9, 3), (0, 3, 8, 5, 1),
    (1, 5, 11, 10, 4), (2, 7, 13, 12, 6), (3, 9, 15, 14, 8), (4, 10, 16, 13, 7),
    (5, 8, 14, 17, 11), (6, 12, 18, 15, 9), (10, 11, 17, 19, 16),
    (12, 13, 16, 19, 18), (14, 15, 18, 19, 17));
    vertexes: ((0, 0, 1.070470), (0.713644, 0, 0.797878),
    (-0.356822, 0.618, 0.797878), (-0.356822, -0.618,
    0.797878), (0.797878, 0.618034, 0.356822), (0.797878, -0.618,
    0.356822), (-0.934172, 0.381966, 0.356822), (0.136294, 1,
    0.356822), (0.136294, -1, 0.356822), (-0.934172, -0.381966,
    0.356822), (0.934172, 0.381966, -0.356822), (0.934172, -0.381966, -0.356822),
    (-0.797878, 0.618, -0.356822), (-0.136294, 1, -0.356822),
    (-0.136294, -1, -0.356822), (-0.797878, -0.618034, -0.356822),
    (0.356822, 0.618, -0.797878), (0.356822, -0.618, -0.797878),
    (-0.713644, 0, -0.797878), (0, 0, -1.070470))));

  FlatPanel: TPolyhedron = (Name: 'panel'; faces: ((3, 0, 1, 2));
    vertexes: ((1, 1, 1), (-1, 1, 1), (-1, -1, 1), (1, -1, 1)));

  StrobeColors: TFaces = ((255, 0, 0), (139, 0, 0), (128, 0, 0),
    (255, 99, 71), (208, 32, 144),
    (219, 112, 147), (199, 21, 133), (205, 92, 92), (178, 34, 34),
    (160, 82, 45), (255, 69, 0), (255, 165, 0), (255, 140, 0), (255, 127, 80
    ), (240, 128, 128), (255, 204, 153), (255, 218, 185), (255, 239, 213),
    (255, 239, 213), (255, 255, 224), (255, 250, 205), (173, 255, 47),
    (255, 204, 51), (218, 165, 32), (184, 134, 11), (238, 221, 130),
    (250, 250, 210), (238, 232, 170), (255, 182, 193), (255, 192, 203),
    (255, 105, 180), (255, 51, 255), (255, 20, 147), (255, 228, 225),
    (250, 128, 114), (255, 160, 122), (233, 150, 122), (154, 205, 50),
    (0, 128, 0), (152, 251, 152), (144, 238, 144), (0, 100, 0), (128, 128, 0
    ), (107, 142, 35), (85, 107, 47), (127, 255, 0), (34, 139, 34),
    (220, 220, 220), (0, 255, 127), (0, 250, 154), (124, 252, 0),
    (240, 255, 240), (245, 255, 250), (32, 178, 170), (60, 179, 113),
    (143, 188, 143), (0, 255, 0), (50, 205, 50), (0, 128, 128),
    (64, 224, 208), (175, 238, 238), (72, 209, 204), (0, 206, 209),
    (51, 255, 255), (102, 205, 170), (0, 0, 255), (0, 0, 205), (0, 0, 139),
    (173, 216, 230), (240, 248, 255), (0, 0, 102), (0, 0, 128),
    (240, 255, 255), (95, 158, 160), (0, 255, 255), (224, 255, 255),
    (0, 139, 139), (106, 90, 205), (132, 112, 255), (123, 104, 238),
    (255, 255, 241), (255, 255, 242), (135, 206, 235), (135, 206, 250),
    (0, 191, 255), (25, 25, 112), (70, 130, 180), (176, 196, 222),
    (65, 105, 225), (100, 149, 237), (30, 144, 255), (255, 255, 243),
    (255, 204, 48), (128, 0, 128), (147, 112, 219), (230, 230, 250),
    (255, 240, 245), (218, 112, 214), (186, 85, 211), (153, 50, 204),
    (216, 191, 216), (221, 160, 221), (238, 130, 238), (148, 0, 211),
    (255, 0, 255), (139, 0, 139), (255, 255, 255), (250, 235, 215),
    (255, 250, 240), (248, 248, 255), (255, 255, 240), (255, 222, 173),
    (255, 250, 250), (245, 245, 245), (169, 169, 169), (128, 128, 128),
    (47, 79, 79), (105, 105, 105), (211, 211, 211), (112, 128, 144),
    (119, 136, 153), (165, 42, 42), (210, 105, 30), (188, 143, 143),
    (139, 69, 19), (244, 164, 96), (255, 245, 238), (245, 245, 220),
    (192, 192, 194), (253, 245, 230), (245, 222, 179), (255, 235, 205),
    (255, 228, 196), (205, 133, 63), (255, 228, 181), (255, 248, 220),
    (222, 184, 135), (240, 230, 140), (189, 183, 107), (0, 0, 0),
    (255, 255, 204), (255, 215, 0), (192, 192, 192), (204, 102, 51));

{-------------------------}
procedure drawPolyLine(pt: TPolyType);
procedure drawPolyLine(p: TPolyhedron);
procedure drawPolyLine(pt: TPolyType; color: TVertex);
procedure drawPolyLine(p: TPolyhedron; color: TVertex);

procedure drawPoly(pt: TPolyType; const colors: TVertexes = nil);
procedure drawPoly(p: TPolyHedron; const colors: TVertexes = nil);

procedure drawPolySolid(pt: TPolyType; txs: TTextures = nil);
procedure drawPolySolid(p: TPolyhedron; txs: TTextures = nil);
procedure drawPolySolid(p: TPolyhedron; tx: TTexture);

procedure drawPolyFaces(pt: TPolyType; faceList: TFace = nil; txs: TTextures = nil);

procedure drawPolyPoints(p: TPolyhedron);

function trackPoly(pt: TPolyType): TVertexes;
function trackPoly(poly: TPolyhedron): TVertexes;

function strobeColor(i: integer): TVertex;
function getStrobeColors(n: integer): TVertexes;

procedure sceneInit;
function polygonCoords(n: integer): TTextureCoords;
function loadTexturesFromPath(path: string; nMax: integer = -1): TTextures;
procedure releaseTextures(const txs: TTextures);

function sphere(rad: realNumber; sec: integer): TPolyhedron;
procedure chrysanthemum;

function mkVertex(x, y, z: realNumber): TVertex;
function mkFace(a, b, c: TFaceIndex): TFace;
function waterman(radius: single): TPolyhedron;

{-------------------------}
implementation


function waterman(radius: single): TPolyhedron;
var
  x, y, z, a, b, c, xra, xrb, yra, yrb, zra, zrb, R, Ry, s, radius2: single;
  i: integer;
begin
  a := 0; // center
  b := 0;
  c := 0;

  s := radius;
  radius2 := radius; // * radius;
  xra := Ceil(a - s);
  xrb := Floor(a + s);

  Result.vertexes := nil;
  Result.normals := nil;
  Result.faces := nil;
  Result.textures := nil;
  Result.Name := 'waterman' + floattostr(radius);

  x := xra;
  while x <= xrb do
  begin
    R := radius2 - Sqr(x - a);
    if R < 0 then
    begin
      x += 1;
      Continue;
    end;
    s := Sqrt(R);
    yra := Ceil(b - s);
    yrb := Floor(b + s);

    y := yra;
    while y <= yrb do
    begin
      Ry := R - Sqr(y - b);
      if Ry < 0 then
      begin
        y += 1;
        Continue; // case Ry < 0
      end;
      if (Ry = 0) and (c = Floor(c)) then
      begin
        if (x + y + c) mod 2 <> 0 then
        begin
          y += 1;
          Continue;
        end
        else
        begin
          zra := c;
          zrb := c;
        end;
      end
      else
      begin // case Ry > 0
        s := Sqrt(Ry);
        zra := Ceil(c - s);
        zrb := Floor(c + s);
        if (x + y) mod 2 = 0 then
        begin // (x+y) mod 2 = 0
          if zra mod 2 <> 0 then
          begin
            if zra <= c then
              zra += 1
            else
              zra -= 1;
          end;
        end
        else
        begin // (x+y) mod 2 <> 0
          if zra mod 2 = 0 then
          begin
            if zra <= c then
              zra += 1
            else
              zra -= 1;
          end;
        end;
      end;

      z := zra;
      while z <= zrb do
      begin
        // save vertex x, y, z
        insert(mkVertex(x, y, z), Result.vertexes, 0);
        z += 2;
      end;
      y += 1;
    end;
    x += 1;
  end;

  // gen faces
  for i := 0 to pred(high(Result.vertexes) div 3) do
    insert(mkFace(i * 3, i * 3 + 1, i * 3 + 2), Result.faces, 0);

end;

function mkEdge(const e1, e2: TFaceIndex): TEdge;
begin
  Result[0] := e1;
  Result[1] := e2;
end;

procedure chrysanthemum;
const
  N = 2000;
var
  u, r, p4, p8, x, y, z: double;
  i: integer;
begin
  glBegin(GL_LINE_LOOP);
  glColor3f(0, 1, 0);

  for i := 0 to N do
  begin
    u := i * 21.0 * Pi / N;
    p4 := sin(17 * u / 3);
    p8 := sin(2 * cos(3 * u) - 28 * u);
    r := 5 * (1 + sin(11 * u / 5)) - (4 * p4 ** 4) * p8 ** 8;

    x := r * cos(u);
    y := r * sin(u);
    z := (r / 20 + 0.2) * sin(r * (2 * Pi) / 7);

    if i > 0 then
      glVertex3f(x, y, z);
  end;
  glEnd;
end;


{ TVertex primitives }
operator -(const x, y: TVertex): TVertex;
begin
  Result[0] := x[0] - y[0];
  Result[1] := x[1] - y[1];
  Result[2] := x[2] - y[2];
end;

operator +(const x, y: TVertex): TVertex;
begin
  Result[0] := x[0] + y[0];
  Result[1] := x[1] + y[1];
  Result[2] := x[2] + y[2];
end;

operator /(const x: TVertex; a: realNumber): TVertex;
begin
  Result[0] := x[0] / a;
  Result[1] := x[1] / a;
  Result[2] := x[2] / a;
end;


function mkVertex(x, y, z: realNumber): TVertex;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function mkFace(a, b, c: TFaceIndex): TFace;
begin
  Result := nil;
  setlength(Result, 3);
  Result[0] := a;
  Result[1] := b;
  Result[2] := c;
end;


function distance(const x, y: TVertex): realNumber;
begin
  Result := sqrt((x[0] - y[0]) ** 2 + (x[1] - y[1]) ** 2 + (x[2] - y[2]) ** 2);
end;

function normalise(x, y, z: realNumber): TVertex;
var
  length: realNumber;
begin
  length := x * x + y * y + z * z;

  if length > 0 then
  begin
    length := sqrt(length);
    x /= length;
    y /= length;
    z /= length;
  end
  else
  begin
    x := 0;
    y := 0;
    z := 0;
  end;

  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function normalise(const x: TVertex): TVertex;
begin
  Result := normalise(x[0], x[1], x[2]);
end;


function calcNormal(const p0, p1, p2: TVertex): TVertex;
var
  xa, ya, za, xb, yb, zb, nx, ny, nz: realNumber;
begin
  xa := p1[0] - p0[0];
  ya := p1[1] - p0[1];
  za := p1[2] - p0[2];

  xb := p2[0] - p0[0];
  yb := p2[1] - p0[1];
  zb := p2[2] - p0[2];

  nx := ya * zb - za * yb;
  ny := za * xb - xa * zb;
  nz := xa * yb - ya * xb;

  Result := normalise(nx, ny, nz);
end;


procedure sceneInit;
const
  lmodel_ambient: vec4f = (0, 0, 0, 0);
  lmodel_twoside: vec1f = (0); // (GL_FALSE);
  light0_ambient: vec4f = (0.6, 0.6, 0.6, 0.6);
  light0_diffuse: vec4f = (0.6, 0.6, 0.6, 0.6);
  light0_position: vec4f = (1, 0.5, 1, 0);
  light1_position: vec4f = (-1, 0.5, -1, 0);
  light0_specular: vec4f = (0.2, 0.2, 0.2, 0.2);
  bevel_mat_ambient: vec4f = (0.3, 0.3, 0, 1);
  bevel_mat_shininess: vec1f = (5);
  bevel_mat_specular: vec4f = (0.1, 0.1, 0.1, 0);
  bevel_mat_diffuse: vec4f = (1, 0, 0, 0);
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT, light0_ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light0_specular);
  glLightfv(GL_LIGHT0, GL_POSITION, light0_position);
  glEnable(GL_LIGHT0);

  glLightfv(GL_LIGHT1, GL_AMBIENT, light0_ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, light0_diffuse);
  glLightfv(GL_LIGHT1, GL_SPECULAR, light0_specular);
  glLightfv(GL_LIGHT1, GL_POSITION, light1_position);
  glEnable(GL_LIGHT1);

  glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
  glEnable(GL_LIGHTING);

  glMaterialfv(GL_FRONT, GL_AMBIENT, bevel_mat_ambient);
  glMaterialfv(GL_FRONT, GL_SHININESS, bevel_mat_shininess);
  glMaterialfv(GL_FRONT, GL_SPECULAR, bevel_mat_specular);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, bevel_mat_diffuse);

  glEnable(GL_COLOR_MATERIAL);
  glShadeModel(GL_SMOOTH);

  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POLYGON_SMOOTH); // creates white shade on lines -> remove;
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
end;

function polygonCoords(n: integer): TTextureCoords;
const
  r = 0.5;
  xoffset = 0.5;
  yoffset = 0.5;
var
  i: integer;
  a, pi2n: realNumber;
begin
  // generate texture polygon coords [u,v] x n

  Result := nil;
  setLength(Result, n);
  pi2n := 2 * PI / n;

  for i := 0 to pred(n) do
  begin
    a := n - i - n / 2;
    Result[i][0] := r * sin(a * pi2n) + xoffset;
    Result[i][1] := r * cos(a * pi2n) + yoffset;
  end;
end;

function loadTexturesFromPath(path: string; nMax: integer): TTextures;
var
  sr: TSearchRec;
  fileName: string;
  tx: TTexture;
begin
  Result := nil;
  if FindFirst(path + '*.*', faAnyFile - faDirectory, sr) = 0 then
  begin
    repeat
      fileName := path + sr.Name;
      if tx.load(fileName) then insert(tx, Result, 0);
      if nmax <> -1 then if length(Result) = nmax then break;
    until FindNext(sr) <> 0;
  end;
end;

procedure releaseTextures(const txs: TTextures);
var
  t: TTexture;
begin
  for t in txs do t.Delete;
end;

function sphere(rad: realNumber; sec: integer): TPolyhedron;
var
  vertexes: TVertexes = nil;
  faces: TFaces = nil;
  textures: TTextureCoords = nil;
  n, n2, nface, cv, cl, cf, r, c, off: integer;
  v, u, du, dv, theta1, theta2, xn, yn, zn, cosRY, sinRY, xt, yt, zt,
  x, y, z: realNumber;
begin
  n := sec + 1;
  n2 := n * n;
  nface := sec * sec;

  SetLength(faces, nface);
  SetLength(vertexes, n2);
  SetLength(textures, n2);

  cv := 0;
  du := 1.0 / sec;
  dv := 1.0 / sec;

  for r := 0 to sec do
  begin
    v := r * dv;
    theta1 := v * pi;

    xn := 0;
    yn := 1;
    zn := 0;

    cosRY := cos(theta1);
    sinRY := sin(theta1);

    xt := -(yn * sinRY);
    yt := +(yn * cosRY);
    zt := 0;

    xn := xt;
    yn := yt;
    zn := zt;

    for c := 0 to sec do
    begin
      u := c * du;
      theta2 := u * pi * 2;

      x := xn;
      y := yn;
      z := zn;

      cosRY := cos(theta2);
      sinRY := sin(theta2);

      xt := (x * cosRY) + (z * sinRY);
      zt := (x * -sinRY) + (z * cosRY);
      x := xt;
      z := zt;

      x *= rad;
      y *= rad;
      z *= rad;

      vertexes[cv][0] := x;
      vertexes[cv][1] := y;
      vertexes[cv][2] := z;

      textures[cv][0] := u;
      textures[cv][1] := v;

      Inc(cv);
    end;
  end;

  {create faces}
  cl := sec + 1;
  cf := 0;

  for r := 0 to sec - 1 do
  begin
    off := r * cl;
    for c := 0 to sec - 1 do
    begin
      SetLength(faces[cf], 4);
      faces[cf][0] := off + c;
      faces[cf][1] := off + c + 1;
      faces[cf][2] := off + c + 1 + cl;
      faces[cf][3] := off + c + 0 + cl;
      Inc(cf);
    end;
  end;


  Result.Name := 'sphere';
  Result.faces := faces;
  Result.vertexes := vertexes;
  Result.textures := textures;
  Result.calcNormals;
end;

procedure drawPolyMode(const poly: TPolyhedron; mode: GLenum;
  const colors: TVertexes = nil);
var
  f: TFaceIndex;
  face: TFace;
  i: integer = 0;
begin
  if length(poly.faces) > 0 then
    for face in poly.faces do
    begin
      glBegin(mode);
      if colors <> nil then
        glColor3fv(colors[(length(face) - 3) mod length(colors)]);

      for f in face do
        glVertex3fv(poly.vertexes[f]);
      glEnd;
      Inc(i);
    end
  else
  begin
    glBegin(GL_LINE_LOOP);
    for i := 0 to high(poly.vertexes) do
      glVertex3fv(poly.vertexes[i]);
    glEnd;

  end;
end;

procedure drawPolyLine(pt: TPolyType; color: TVertex);
begin
  drawPolyMode(Polyhedrons[qword(pt)], GL_LINE_LOOP, [color]);
end;

procedure drawPolyLine(p: TPolyhedron; color: TVertex);
begin
  drawPolyMode(p, GL_LINE_LOOP, [color]);
end;

procedure drawPoly(pt: TPolyType; const colors: TVertexes = nil);
begin
  drawPolyMode(Polyhedrons[qword(pt)], GL_POLYGON, colors);
end;

procedure drawPoly(p: TPolyHedron; const colors: TVertexes);
begin
  drawPolyMode(p, GL_POLYGON, colors);
end;

procedure drawPolyLine(pt: TPolyType);
begin
  drawPolyMode(Polyhedrons[qword(pt)], GL_LINE_LOOP);
end;

procedure drawPolyLine(p: TPolyhedron);
begin
  drawPolyMode(p, GL_LINE_LOOP);
end;

procedure drawPolySolid(pt: TPolyType; txs: TTextures);
begin
  drawPolySolid(Polyhedrons[qword(pt)], txs);
end;

procedure drawPolySolid(p: TPolyhedron; txs: TTextures);
var
  face: TFace;
  c, it: integer;
  txc: TTextureCoords;
begin
  it := 0;

  for face in p.faces do
  begin
    if txs <> nil then txs[it mod length(txs)].bind;
    Inc(it);

    glBegin(GL_POLYGON);
    txc := polygonCoords(length(FACE));

    for c := 0 to high(txc) do
    begin
      glTexCoord2fv(txc[c]);
      glVertex3fv(p.vertexes[face[c]]);
    end;
    glEnd;
  end;
end;

procedure drawPolySolid(p: TPolyhedron; tx: TTexture);
var
  face: TFace;
  c: integer;
  polyCoord: TTextureCoords = nil;
begin
  glColor3f(1, 1, 1);
  tx.bind;

  for face in p.faces do
  begin
    glBegin(GL_POLYGON);

    if p.textures = nil then polyCoord := polygonCoords(length(face));

    for c := 0 to high(face) do
    begin
      if p.normals <> nil then glNormal3fv(p.normals[face[c]]);
      if p.textures <> nil then glTexCoord2fv(p.textures[face[c]])
      else
        glTexCoord2fv(polyCoord[c]);

      glVertex3fv(p.vertexes[face[c]]);
    end;

    glEnd;
  end;
end;

procedure drawPolyFaces(pt: TPolyType; faceList: TFace; txs: TTextures);
var
  f, c, it: integer;
  poly: TPolyhedron;
  txc: TTextureCoords;
  face: TFace;
begin
  poly := Polyhedrons[qword(pt)];

  if faceList = nil then drawPolySolid(pt, txs)
  else
  begin
    it := 0;

    for f := 0 to high(faceList) do
    begin
      face := poly.faces[f];
      if txs <> nil then txs[it mod length(txs)].bind;
      Inc(it);

      glBegin(GL_POLYGON);
      txc := polygonCoords(length(face));

      for c := 0 to high(txc) do
      begin
        glTexCoord2fv(txc[c]);
        glVertex3fv(poly.vertexes[face[c]]);
      end;
      glEnd;
    end;
  end;
end;

procedure drawPolyPoints(p: TPolyhedron);
var
  v: TVertex;
begin
  glBegin(GL_POINTS);
  for v in p.vertexes do glVertex3fv(v);
  glEnd;
end;

function trackPoly(pt: TPolyType): TVertexes;
begin
  Result := trackPoly(Polyhedrons[qword(pt)]);
end;

function trackPoly(poly: TPolyhedron): TVertexes;
var
  face: TFace;
  f: TFaceIndex;
  edges: TEdges;

  procedure insVertex(fc1, fc2: TFaceIndex);
  var
    e: TEdge;
  begin
    fc1 := face[fc1];
    fc2 := face[fc2];

    { lookup for dupes edges }
    for e in edges do
      if ((e[0] = fc1) and (e[1] = fc2)) or ((e[0] = fc2) and (e[1] = fc1)) then
        exit; // found dupe -> quit

    insert(poly.vertexes[fc1], Result, 0);
    insert(poly.vertexes[fc2], Result, 0);

    insert(mkEdge(fc1, fc2), edges, 0);
  end;

begin
  Result := nil;
  edges := nil;

  for face in poly.faces do
  begin
    for f := 0 to high(face) - 1 do
      insVertex(f, f + 1);
    insVertex(high(face), 0); // close loop
  end;
end;

function strobeColor(i: integer): TVertex;

  function intCol2Single(ic: TFace): TVertex;
  begin
    Result[0] := ic[0] / 255;
    Result[1] := ic[1] / 255;
    Result[2] := ic[2] / 255;
  end;

begin
  Result := intCol2Single(StrobeColors[i mod length(StrobeColors)]);
end;

function getStrobeColors(n: integer): TVertexes;
var
  i: integer;
begin
  Result := nil;
  setLength(Result, n);
  for i := 0 to pred(n) do Result[i] := strobeColor(random(length(StrobeColors)));
end;

{ TPolyhedron }

procedure TPolyhedron.calcNormals;
var
  f: TFaceIndex;
  nrm: TVertex;
  face: TFace;
begin
  setLength(normals, length(Vertexes));
  for face in faces do
  begin
    nrm := calcNormal(vertexes[face[0]], vertexes[face[1]], vertexes[face[2]]);
    for f in face do
      normals[f] := nrm;
  end;
end;

procedure TPolyhedron.calcTextures;
var
  i, f, c: integer;
  txc: TTextureCoords;
begin
  setLength(textures, length(vertexes));
  for i := 0 to high(faces) do
  begin
    txc := polygonCoords(length(faces[i]));
    c := 0;
    for f in faces[i] do
    begin
      textures[f] := txc[c];
      Inc(c);
    end;
  end;
end;



{ TTexture }

function TTexture.load(_fileName: string): boolean;
var
  pic: TPicture;
begin
  pic := TPicture.Create;
  pic.LoadFromFile(_fileName);
  fileName := _fileName;

  load(pic);
  pic.Free;

  Result := True;
end;

function TTexture.load(pic: TPicture): boolean;

  function swapBR(size: integer; pimg: pbyte): pbyte;   // swap blue <-> red

    procedure swap(var a: byte; var b: byte);
    var
      t: byte;
    begin
      t := a;
      a := b;
      b := t;
    end;

  var
    i, c: integer;
  begin
    c := 0;
    Result := pimg;
    for i := 0 to pred(size) do
    begin // rgba -> bgra
      swap(pimg[c], pimg[c + 2]);
      Inc(c, 4); // rgba
    end;
  end;

begin
  glGenTextures(1, @tID);
  glBindTexture(GL_TEXTURE_2D, tID);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  Result := False;

  // Texture blends with object background
  with pic do
  begin
    Result := True;

    with BitMap do
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA,
        GL_UNSIGNED_BYTE, swapBR(Width * Height, RawImage.Data));

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end;
end;

procedure TTexture.bind;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, tID);
end;

procedure TTexture.enable;
begin
  glEnable(GL_TEXTURE_2D);
end;

procedure TTexture.disable;
begin
  glDisable(GL_TEXTURE_2D);
end;

procedure TTexture.Delete;
begin
  glDeleteTextures(1, @tID);
end;

{ TGL }

constructor TGL.Create(_parent: TWinControl; _withLists: boolean);
begin
  inherited Create(_parent);

  parent := _parent;

  Align := alClient;

  OnPaint := @glPaint;

  parent.OnResize := @glResize;
  OnMouseMove := @glMouseMove;
  OnMouseDown := @glMouseDown;
  OnMouseUp := @glMouseUp;
  OnMouseWheel := @glMouseWheel;

  anglex := 0;
  angley := 0;
  offx := 0;
  offy := 0;

  scale := -5;
  _msDown := False;

  firstTime := True;

  dispList := -1;
  withLists := _withLists;
end;

destructor TGL.Destroy;
begin
  if dispList <> -1 then glDeleteLists(dispList, 1);
  inherited Destroy;
end;

procedure TGL.init;
begin
end;

procedure TGL.draw;
begin
end;

procedure TGL.identity;
begin
  glLoadIdentity();
  glTranslatef(offx, offy, scale);
end;

procedure TGL.identity(ax, ay: realNumber);
begin
  identity;
  glRotatef(ax, 1, 0, 0);  // scenario panel
  glRotatef(ay, 0, 1, 0);
end;

procedure TGL.incAngles(ax, ay: realNumber);
begin
  anglex += ax;
  angley += ay;

  if anglex > 360 then anglex -= 360;
  if angley > 360 then angley -= 360;

  refresh;
end;

{ call list support }
procedure TGL.startList;
begin
  dispList := glGenLists(1);
  glNewList(dispList, GL_COMPILE);
end;

procedure TGL.endList;
begin
  glEndList;
end;

procedure TGL.useList;
begin
  glCallList(dispList);
end;

procedure TGL.newList;
begin
  if withLists and (dispList <> -1) then
  begin
    glDeleteLists(dispList, 1);
    dispList := -1;
  end;
end;

procedure TGL.useLists;
begin
  withLists := True;
end;

{ internal paint, calls user draw}
procedure TGL.glPaint(Sender: TObject);

  procedure preparePaint;
  begin
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // Clear color and depth buffers
    glLoadIdentity(); // # Reset the model-view matrix

    glTranslatef(offx, offy, scale);  // scale & rot.
    glRotatef(anglex, 1, 0, 0);
    glRotatef(-angley, 0, 1, 0);
  end;

begin
  glInit;  // only once
  preparePaint;

  if withLists then
  begin
    if dispList = -1 then
    begin
      startList;
      draw;
      endList;
    end;

    useList;
  end
  else
    draw;

  SwapBuffers;
end;

procedure TGL.glInit;
begin
  if firstTime then  // do this only once
  begin
    glClearColor(0, 0, 0, 0);
    glEnable(GL_DEPTH_TEST);

    glResize(self);

    firstTime := False;

    init;
  end;
end;

procedure TGL.glResize(Sender: TObject);

  procedure perspectiveGL(fovY, aspect, zNear, zFar: double);
  var
    fH, fW: double;
  begin
    fH := tan(fovY / 360 * PI) * zNear;
    fW := fH * aspect;
    glFrustum(-fW, fW, -fH, fH, zNear, zFar);
  end;

var
  w, h, md: integer;
begin

  w := Width;
  h := Height;

  md := min(w, h);

  if w > h then
  begin
    offx := 1 / md * (w - h) / 2;
    offy := 0;
  end
  else
  begin
    offx := 0;
    offy := 1 / md * (h - w) / 2;
  end;

  glViewport(0, 0, md, md);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  perspectiveGL(45, 1, 1 / md, 1000);
  glMatrixMode(GL_MODELVIEW);

  Invalidate;
end;

procedure TGL.glMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft: _msDown := True;
    mbRight: Invalidate;
    else;
  end;
end;

procedure TGL.glMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
const
  d = 2.0;
begin
  if _msDown then
  begin

    angley := angley + (x - lastx) / d;
    anglex := anglex + (y - lasty) / d;

    lastx := X;
    lasty := Y;

    Invalidate;
  end;
end;

procedure TGL.glMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  _msDown := False;
end;

procedure TGL.glMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  scale := scale + WheelDelta / 400;
  Invalidate;
end;

{ TTube }

constructor TTube.Create(_track: TVertexes; _rad: realNumber;
  slices: integer; _color: TVertex);
var
  i: integer;
begin
  color := _color;
  track := _track;
  rad := _rad;

  _tubes := nil;
  for i := 0 to pred(length(track) div 2) do // from, to pairs
    insert(doTube(track[i * 2], track[i * 2 + 1], rad, slices), _tubes, 0);
end;

function TTube.doTube(_from, _to: TVertex; r: realNumber; slices: integer): TPolyhedron;
const
  HALF_PI = PI / 2;
var
  i, j: integer;
  PIs, len, heading, sq, pitch, x, y, z: realNumber;

  vec, mid, nrm: TVertex;
  q: TQuad;
begin

  PIs := 2 * PI / slices;
  len := distance(_from, _to) / 2;

  vec := _to - _from;
  mid := (_to + _from) / 2;

  heading := -arctan2(vec[2], vec[0]);
  sq := sqrt((vec[0] * vec[0]) + (vec[2] * vec[2]));
  pitch := arctan2(vec[1], sq);

  Result.vertexes := nil;
  Result.normals := nil;

  for j := 0 to slices do
  begin    // rod is horizontal along X to start
    q[0] := mkVertex(len, r * cos(j * PIs), r * sin(j * PIs));
    q[1] := mkVertex(-len, r * cos(j * PIs), r * sin(j * PIs));
    q[2] := mkVertex(-len, r * cos((j + 1) * PIs), r * sin((j + 1) * PIs));
    q[3] := mkVertex(len, r * cos((j + 1) * PIs), r * sin((j + 1) * PIs));

    for i := 0 to 3 do // pitch
    begin
      x := q[i][0];
      y := q[i][1];
      q[i][0] := x * cos(pitch) + y * cos(pitch + HALF_PI);
      q[i][1] := x * sin(pitch) + y * sin(pitch + HALF_PI);
    end;

    for i := 0 to 3 do // heading
    begin
      z := q[i][2];
      x := q[i][0];
      q[i][2] := z * cos(heading) + x * cos(heading + HALF_PI);
      q[i][0] := z * sin(heading) + x * sin(heading + HALF_PI);
    end;

    for i := 0 to 3 do   // move tube so that it goes from one end to the other.
      q[i] += mid;

    nrm := calcNormal(q[0], q[1], q[2]);
    for vec in q do
    begin  // insert the quad (mkVertex, normals)
      insert(vec, Result.vertexes, 0);
      insert(nrm, Result.normals, 0);
    end;
  end;
end;

procedure TTube.draw;
var
  t, sp: TPolyhedron;
  v: TVertex;
  i, j: integer;
begin
  glDisable(GL_TEXTURE_2D);
  glColor3fv(color);

  for t in tubes do {tubes}
  begin
    for i := 0 to pred(length(t.vertexes) div 4) do  // quads
    begin
      glBegin(GL_QUADS);

      for j := 0 to 3 do
      begin
        glNormal3fv(t.normals[i * 4 + j]);
        glVertex3fv(t.vertexes[i * 4 + j]);
      end;

      glEnd;
    end;
  end;

  sp := sphere(rad * 1.3, 10);
  for i := 0 to pred(length(track) div 2) do  {spheres in mkVertex corners}
  begin
    glPushMatrix;

    begin
      v := track[i * 2];
      glTranslatef(v[0], v[1], v[2]);
      drawPolySolid(sp);
    end;

    glPopMatrix;
  end;

end;

initialization
  randomize;
end.
