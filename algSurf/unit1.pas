unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, OpenGLContext, gl,
  Types, Math, StdCtrls, ExtCtrls, Spin, ComCtrls;

// link with 'c' and libengine.a
{$linklib c}
{$linklib nim/engine.a}

const
  asNames: array of string = ('cap', 'boy', 'roman', 'sea shell',
    'tudor rose', 'breather',
    'klein bottle', 'klein bottle 0', 'bour', 'dini', 'enneper',
    'scherk', 'conical spiral', 'bohemian dome', 'astrodial ellipse',
    'apple', 'ammonite', 'plucker comoid', 'cayley', 'up down shell',
    'butterfly', 'rose', 'kuen',
    'tanaka-0', 'tanaka-1', 'tanaka-2', 'tanaka-3');

type
  TVec3 = record
    x, y, z: single;
  end;

  TQuadNormal = record
    qv: array [0..3] of TVec3;
    normal: TVec3;
  end;

  pTQuadNormal = ^TQuadNormal;


  { TForm1 }

  TForm1 = class(TForm)
    lbNames: TListBox;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    resolution: TSpinEdit;
    StatusBar1: TStatusBar;

    procedure FormActivate(Sender: TObject);
    procedure lbNamesClick(Sender: TObject);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure resolutionChange(Sender: TObject);

  private

    nas, resol: integer;
    vertexes: array of TQuadNormal;

    anglex, angley, scale: single;
    lastx, lasty: integer;
    down: boolean;

    procedure sceneInit;
    procedure loadAS;
  public

  end;

// alg surf
procedure algSurf(nas, resol: integer; quadNormals: pTQuadNormal); cdecl; external;
function getNVertexes(resol: integer): integer; cdecl; external;
function getQuadNormalSize: integer; cdecl; external;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.loadAS;
begin
  resol := resolution.Value;

  nas := lbNames.ItemIndex;
  if nas = -1 then nas := 0;

  assert(getQuadNormalSize = sizeof(TQuadNormal), 'nim & fpc sizeof TQuadNormal are different');

  setLength(vertexes, getNVertexes(resol));
  algSurf(nas, resol, @vertexes[0]);

  StatusBar1.SimpleText:=format('%0s, resolution:%1d, vertexes:%2d ',[asNames[nas], resol, getNVertexes(resol)]);;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  n: string;
begin

  //  file lb
  for n in asNames do lbNames.AddItem(n, nil);
  scale := 0;

  loadAS;
end;

procedure TForm1.lbNamesClick(Sender: TObject);
begin
  loadAS;
  OpenGLControl1.Invalidate;
end;

procedure TForm1.sceneInit;
type
  vec4f = array[0..3] of single;
  vec1f = array[0..0] of single;
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

procedure TForm1.OpenGLControl1Resize(Sender: TObject);

  procedure perspectiveGL(fovY, aspect, zNear, zFar: double);
  var
    fH, fW: double;
  begin
    fH := tan(fovY / 360 * PI) * zNear;
    fW := fH * aspect;
    glFrustum(-fW, fW, -fH, fH, zNear, zFar);
  end;

var
  w, h: integer;
begin
  w := OpenGLControl1.Width;
  h := OpenGLControl1.Height;

  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  perspectiveGL(45, w / h, 1, 1000);
  glMatrixMode(GL_MODELVIEW);

  OpenGLControl1.Invalidate;
end;

procedure TForm1.resolutionChange(Sender: TObject);
begin
  loadAS;
  OpenGLControl1.Invalidate;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
var
  qn : TQuadNormal;
  v : TVec3;

begin
  // drawPoly(poly);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // Clear color and depth buffers
  glMatrixMode(GL_MODELVIEW); // # To operate on model-view matrix
  glLoadIdentity(); // # Reset the model-view matrix

  if resol <> 0 then
  begin
    //OpenGLControl1Resize(nil);

    sceneInit;
    glColor3f(0.5, 0.5, 0.1);

    glTranslatef(0, 0, scale);
    glRotatef(anglex, 1, 0, 0);
    glRotatef(-angley, 0, 1, 0);

    for qn in vertexes do
    begin
      glBegin(GL_QUADS);
      glNormal3fv(@qn.normal);
      for v in qn.qv do glVertex3fv(@v);
      glEnd();
    end;

    OpenGLControl1.SwapBuffers;

  end;
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
  scale := scale + WheelDelta / 400;
  OpenGLControl1.Invalidate;
end;

end.
