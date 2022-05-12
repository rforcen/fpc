unit Unit1;

{$mode objfpc}{$H+}



interface

uses
  Classes, SysUtils, OpenGLContext, GL, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, gcontnrs, Types, common, ffiwrapper, DateUtils, colorMap;

{ TForm1 }
type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);

    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, Y: integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);
  private

    facesLined: array of integer;
    faces: TFaces;
    vertexes: array of TPoint3d;
    nfaces, nvertexes, maxNFaces: integer;
    colMap: TColorMap;

    t0: TDateTime;
    lap: int64;

    procedure setColorMap;

  private

    anglex, angley, scale: single;
    lastx, lasty: integer;
    down: boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  faces := TFaces.Create;
  colMap := TColorMap.Create;
  scale := 0.6;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  faces.Free;
  releaseWatermanMesh;
  colMap.Free;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
var
  face: TVecInt;
  ix: integer;
  v: TPoint3d;

  procedure draw(mode: integer);
  begin
    for face in faces do
    begin

      glBegin(mode);

      v := colMap[length(face)];
      if mode = GL_POLYGON then
        glColor3dv(@v);

      for ix in face do
        glVertex3dv(@vertexes[ix]);

      glEnd();
    end;
  end;

begin

  glClearColor(0.0, 0.0, 0.0, 1.0); // Set background color to black and opaque
  glClearDepth(1.0); // Set background depth to farthest
  glEnable(GL_DEPTH_TEST); // Enable depth testing for z-culling
  glDepthFunc(GL_LEQUAL); // Set the type of depth-test
  glShadeModel(GL_SMOOTH); // Enable smooth shading
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);  // Nice perspective corrections

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  //sceneInit;
  glLoadIdentity;

  glRotatef(anglex, 1, 0, 0);
  glRotatef(-angley, 0, 1, 0);
  glColor3f(1, 1, 1);
  glScalef(0.7, 0.7, 0.7);


  if colMap.Size > 0 then
  begin
    draw(GL_POLYGON);
    glColor3f(0, 0, 0);
    draw(GL_LINE_LOOP);
  end;


  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  randomize;
  TrackBar1.position := random(100);
  TrackBar1Change(nil);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  setColorMap;
  OpenGLControl1.Invalidate;
end;

procedure TForm1.setColorMap;
var
  face: TFace;
begin
  colMap.Clear;

  for face in faces do
    if not colMap.Contains(length(face)) then
      colMap[length(face)] := mkp3d(random, random, random);

end;

procedure TForm1.TrackBar1Change(Sender: TObject);

  procedure deLineFaces;
  var
    i, ifc, currLen: integer;
    fc: TVecInt = nil;
  begin

    faces.Clear;

    ifc := 0;
    currLen := facesLined[0];
    setLength(fc, currLen);

    for i := 1 to high(facesLined) do
    begin

      if ifc = currLen then
      begin
        faces.append(fc);
        currLen := facesLined[i];
        setLength(fc, currLen);
        ifc := 0;
      end
      else
      begin
        fc[ifc] := facesLined[i];
        Inc(ifc);
      end;

    end;
    faces.append(fc);

  end;

var
  radius: single;
begin
  radius := TrackBar1.Position;

  t0 := now;
  generateWaterman(radius, nfaces, nvertexes, maxNFaces);
  // generate waterman poly in convexhull
  lap := MilliSecondsBetween(now, t0);

  setLength(facesLined, nfaces); // space for mesh
  setLength(vertexes, nvertexes);

  getWatermanMesh(@facesLined[0], @vertexes[0]);  // get mesh(faces, vertexes)

  deLineFaces;

  StatusBar1.SimpleText := format('vertexes:%0d, faces:%1d, maxnfaces:%2d, lap:%3dms',
    [nvertexes, nfaces, maxNfaces, lap]);

  setColorMap;
  OpenGLControl1.Invalidate;
end;

procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft: down := True;
    mbRight:
    begin
      OpenGLControl1.Invalidate;
    end;
    else;
  end;
end;

procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
const
  d = 2.0;
begin
  if down then
  begin
    angley := angley + (x - lastx) / d;
    anglex := anglex + (y - lasty) / d;

    lastx := X;
    lasty := Y;

    OpenGLControl1.Invalidate;
  end;
end;

procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  down := False;
end;

procedure TForm1.OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  scale := scale + WheelDelta / 1000;

  OpenGLControl1.Invalidate;
end;

end.
