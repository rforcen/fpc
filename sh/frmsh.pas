unit frmSH;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, MaskEdit, OpenGLContext, GL, uSphericalHarmonics, Types,
  DateUtils, v3, shCodes;

type

  { TFormSH }

  TFormSH = class(TForm)
    btSave: TBitBtn;
    cbCodes: TComboBox;
    cbResol: TComboBox;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    procedure btSaveClick(Sender: TObject);
    procedure cbCodesSelect(Sender: TObject);
    procedure cbResolSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure OpenGLControl1Paint(Sender: TObject);

  private

    sh: SphericalHarmonics;
    anglex, angley, scale: single;
    lastx, lasty: integer;
    down: boolean;
    resolution, code: integer;

    lap : integer;
    t0 : TTime;

    procedure drawSH;
    procedure sceneInit;
    procedure CreateSH;

  public

  end;

var
  FormSH: TFormSH;

implementation

{$R *.lfm}

{ TFormSH }

procedure TFormSH.FormCreate(Sender: TObject);
var
  shcode: integer;

begin
  down := False;
  scale := 0.7;
  resolution := 256;
  code := 220;

  CreateSH;

  for shcode in SphericalHarmonicsCodes do
    cbCodes.AddItem(IntToStr(shcode), nil);

  cbCodes.ItemIndex := 0;
end;

procedure TFormSH.sceneInit;
type
  vec4f = array[0..3] of single;
  vec1f = array[0..0] of single;
const
  lmodel_ambient: vec4f = (0, 0, 0, 0);
  lmodel_twoside: vec1f = (0); // (GL_FALSE);
  light0_ambient: vec4f = (0.3, 0.3, 0.3, 0.4);
  light0_diffuse: vec4f = (0.3, 0.3, 0.3, 0);
  light0_position: vec4f = (1, 0.5, 1, 0);
  light1_position: vec4f = (-1, 0.5, -1, 0);
  light0_specular: vec4f = (0.2, 0.2, 0.2, 0.2);
  bevel_mat_ambient: vec4f = (0.1, 0.1, 0, 1);
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

procedure TFormSH.drawSH;
var
  f, v: integer;
  face: Trig;
  vx: Vertex;
begin
  for f := low(sh.amesh.trigs) to high(sh.amesh.trigs) do
  begin
    face := sh.amesh.trigs[f];
    glBegin(GL_TRIANGLES);
    for v := 0 to 2 do
    begin
      vx := sh.amesh.shape[face[v]];

      glNormal3fv(vx.norm);
      glColor3fv(vx.color);
      glVertex3fv(vx.pos / sh.maxVal * scale);
    end;
    glEnd;
  end;
end;

procedure TFormSH.OpenGLControl1Paint(Sender: TObject);

begin
  glClearColor(0.27, 0.53, 0.71, 1.0); // Set blue background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  sceneInit;
  glLoadIdentity;

  glRotatef(anglex, 1, 0, 0);
  glRotatef(-angley, 0, 1, 0);

  drawSH;

  OpenGLControl1.SwapBuffers;
end;


procedure TFormSH.CreateSH;
begin
  sh.Free;
  t0:=now;

  sh := SphericalHarmonics.Create(resolution, code, 0);

  lap := MilliSecondsBetween(now, t0);
  StatusBar1.SimpleText:=format('lap:%0dms | %1dx%1d = %2d vertex', [lap, sh.n,sh.n, sh.n*sh.n]);
  OpenGLControl1.DoOnPaint
end;

procedure TFormSH.cbCodesSelect(Sender: TObject);
begin
  code := cbCodes.ItemIndex;
  CreateSH
end;

procedure TFormSH.btSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    sh.writePLYStream(SaveDialog1.FileName)
end;

procedure TFormSH.cbResolSelect(Sender: TObject);
begin
  resolution := StrToInt(cbResol.Text);

  CreateSH
end;

procedure TFormSH.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  down := True
end;

procedure TFormSH.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
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

    OpenGLControl1.DoOnPaint
  end;
end;

procedure TFormSH.OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  down := False
end;

procedure TFormSH.OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  scale := scale + WheelDelta / 4000.0;
  OpenGLControl1.DoOnPaint
end;

end.
