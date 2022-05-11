unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, gl, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, DateUtils, Types, ComCtrls, v3, upolygonizer, implicitFuncs, uMesh;
//, uNim, uCpp;

{ TForm1 }
type
  TForm1 = class(TForm)
    btnSave: TButton;
    fsBounds: TFloatSpinEdit;
    lbNames: TListBox;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    seResol: TSpinEdit;
    StatusBar1: TStatusBar;

    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fsBoundsChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure lbNamesClick(Sender: TObject);

    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, Y: integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);

    procedure OpenGLControl1Paint(Sender: TObject);
    procedure seResolClick(Sender: TObject);

  private
    polyg: TPolygonizer;

    resol: integer;
    nfunc: integer;
    bounds: single;

    t0: TDateTime;
    lap: int64;
    isInit: boolean;

  private

    anglex, angley, scale: single;
    lastx, lasty: integer;
    down: boolean;

  private
    //procedure doPoly_nim;
    //procedure doPoly_cpp;
    procedure doPoly;
    procedure sceneInit;
    procedure dispStat;
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
  scale := 0.6;

  // list box with function names
  for nm in FuncNames do lbNames.addItem(nm, nil);
  doPoly;
end;

procedure TForm1.fsBoundsChange(Sender: TObject);
begin
  doPoly;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  if isInit then
  begin
    polyg.writeCTM('implsurf.ctm');
    statusbar1.SimpleText := 'implsurf.ctm file saved';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  isInit := False;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    polyg.free;
end;

procedure TForm1.lbNamesClick(Sender: TObject);
begin
  doPoly;
end;

procedure TForm1.dispStat;
begin
  statusbar1.SimpleText := format('%0d trigs, %1d vertexes, scale:%2f, lap:%3dms',
    [polyg.mesh.shape.Size, polyg.mesh.trigs.Size, scale, lap]);
end;

procedure TForm1.doPoly;
begin
  resol := seResol.Value;
  nfunc := lbNames.ItemIndex;
  if nfunc = -1 then nfunc := 0;
  bounds := fsBounds.Value;
  if bounds = 0 then bounds := 0.1;

  polyg.free;

  t0 := now;

  polyg := TPolygonizer.Create(bounds, resol, ImplicitFunctions[nfunc]);

  lap := MilliSecondsBetween(now, t0);


  dispStat;
  isInit := True;

  OpenGLControl1.Invalidate;
end;


procedure TForm1.sceneInit;
type
  vec4f = array[0..3] of single;
  vec1f = array[0..0] of single;
const
  lmodel_ambient: vec4f = (0, 0, 0, 0);
  lmodel_twoside: vec1f = (0); // (GL_FALSE);
  light0_ambient: vec4f = (0.6, 0.6, 0.6, 0.7);
  light0_diffuse: vec4f = (0.5, 0.5, 0.5, 0.5);
  light0_position: vec4f = (1, 0.5, 1, 0);
  light1_position: vec4f = (-1, 0.5, -1, 0);
  light0_specular: vec4f = (0.3, 0.3, 0.3, 0.3);
  bevel_mat_ambient: vec4f = (0.3, 0.3, 0.3, 1);
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
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
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

begin
  if not isInit then exit;

  glClearColor(0, 0, 0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  sceneInit;
  glLoadIdentity;

  glRotatef(anglex, 1, 0, 0);
  glRotatef(-angley, 0, 1, 0);
  glColor3f(0.5, 0.5, 0.1); // gold

  for trig in polyg.mesh.trigs do
  begin

    glBegin(GL_TRIANGLES);

    for ix in trig.toVec3u do
    begin
      vx := polyg.mesh.shape[ix];

      glNormal3fv(vx.norm);
      glColor3fv(vx.color);
      glVertex3fv(vx.pos * scale / polyg.scale);
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
  scale := scale + WheelDelta / 1000;
  dispStat;
  OpenGLControl1.Invalidate;
end;

end.
