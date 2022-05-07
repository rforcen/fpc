unit Unit1;

{$mode objfpc}{$H+}

// link with 'c' and libengine.a
{$linklib c}
{$linklib nim/polygonizer.a}
{$linklib openctm}

interface

uses
  Classes, SysUtils, OpenGLContext, gl, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, Math, Types, ComCtrls;

{ polygonizer interface }
const
  FuncNames: array of string =
    ('Sphere', 'Blob', 'NordstarndWeird', 'DecoCube', 'Cassini',
    'Orth', 'Orth3', 'Pretzel', 'Tooth', 'Pilz', 'Bretzel', 'BarthDecic',
    'Clebsch0', 'Clebsch', 'Chubs', 'Chair', 'Roman', 'TangleCube', 'Goursat', 'Sinxyz');

type
  TPolygonizer = pointer;
  PChar = ^char;

  TVec3 = record
    x, y, z: single;
  end;

  TVertex = record
    pos, norm, uv, color: TVec3;
  end;

  TTrig = array[0..2] of integer;

function newPolygonizer(bounds: single; idiv: integer; funcidx: integer): TPolygonizer;
  cdecl; external;
procedure writeCTM(polyg: TPolygonizer; Name: PChar); cdecl; external;
function getNVertex(polyg: TPolygonizer): integer; cdecl; external;
function getNTrigs(polyg: TPolygonizer): integer; cdecl; external;
procedure getMesh(polyg: TPolygonizer; vertexes: pointer; trigs: pointer);
  cdecl; external;

{ TForm1 }
type
  TForm1 = class(TForm)
    lbNames: TListBox;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    seResol: TSpinEdit;
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
    procedure seResolClick(Sender: TObject);

  private
    polyg: TPolygonizer;
    vertexes: array of TVertex;
    trigs: array of TTrig;

    resol: integer;
    nfunc: integer;
    bounds: single;

  private

    anglex, angley, scale: single;
    lastx, lasty: integer;
    down: boolean;

  private
    procedure doPoly;
    procedure sceneInit;
  public

  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
var
  nm: string;
begin
  scale := 0;
  // list box with function names
  for nm in FuncNames do lbNames.addItem(nm, nil);

  //writeCTM(polyg, '6.ctm');
  doPoly;
end;

procedure TForm1.lbNamesClick(Sender: TObject);
begin
  doPoly;
end;

procedure TForm1.doPoly;
begin
  resol := seResol.Value;
  nfunc := lbNames.ItemIndex;
  if nfunc = -1 then nfunc := 0;
  bounds := 2;

  polyg := newPolygonizer(bounds, resol, nfunc);
  setLength(vertexes, getNVertex(polyg));
  setLength(trigs, getNTrigs(polyg));
  getMesh(polyg, @vertexes[0], @trigs[0]);

  statusbar1.SimpleText := format('%0d trigs, %1d vertexes',
    [getNTrigs(polyg), getNVertex(polyg)]);

  OpenGLControl1.Invalidate;
end;

procedure TForm1.sceneInit;
type
  vec4f = array[0..3] of single;
  vec1f = array[0..0] of single;
const
  lmodel_ambient: vec4f = (0, 0, 0, 0);
  lmodel_twoside: vec1f = (0); // (GL_FALSE);
  light0_ambient: vec4f = (0.2, 0.2, 0.2, 0.2);
  light0_diffuse: vec4f = (0.2, 0.2, 0.2, 0.2);
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

procedure TForm1.seResolClick(Sender: TObject);
begin
  doPoly;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
var
  trig: TTrig;
  ix: integer;
  vx: TVertex;
  v: TVec3;

begin
  // drawPoly(poly);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // Clear color and depth buffers
  glMatrixMode(GL_MODELVIEW); // # To operate on model-view matrix
  glLoadIdentity(); // # Reset the model-view matrix

  sceneInit;
  glColor3f(0.5, 0.5, 0.2);

  glTranslatef(0, 0, scale);
  glRotatef(anglex, 1, 0, 0);
  glRotatef(-angley, 0, 1, 0);
  glScalef(0.4, 0.4, 0.4);

  for trig in trigs do
  begin

    glBegin(GL_TRIANGLES);

    for ix in trig do
    begin
      vx := vertexes[ix];

      glNormal3fv(@vx.norm);
      //glColor3fv(@vx.color);

      glVertex3fv(@vx.pos);
    end;
    glEnd();
  end;

  OpenGLControl1.SwapBuffers;

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
