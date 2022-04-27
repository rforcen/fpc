unit ui_johnson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, OpenGLContext, gl, Types, Math, uPoly, uCommon,
  uJohnson, DateUtils, uPlato, uTrans;

type

  { TForm1 }

  TForm1 = class(TForm)
    randomPoly: TButton;
    polyDef: TEdit;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    transform: TEdit;
    StatusBar1: TStatusBar;
    procedure randomPolyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure OpenGLControl1MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, Y: integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);

    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure polyDefEditingDone(Sender: TObject);

  private
    anglex, angley, scale: single;
    lastx, lasty: integer;
    down: boolean;

    t0: TDateTime;
    lap: integer;

    poly: CPoly;

    procedure sceneInit;
    procedure genPoly;
    procedure dispStat;
  end;



var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.sceneInit;
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

procedure TForm1.OpenGLControl1Paint(Sender: TObject);

var

  face: TFace;
  ix, ixf: integer;

  procedure drawFaces;
  begin
    ixf := 0;
    for face in poly.faces do
    begin
      glBegin(GL_POLYGON);

      for ix in face do
      begin
        glNormal3fv(@poly.normals[ixf][0]);
        glColor3fv(@poly.colors[ixf][0]);

        glVertex3fv(@poly.vertexes[ix][0]);
      end;
      Inc(ixf);
      glEnd;
    end;
  end;

  procedure drawLines;
  begin
    glColor3f(1, 0, 0);
    for face in poly.faces do
    begin
      glBegin(GL_LINE_LOOP);
      for ix in face do
        glVertex3fv(@poly.vertexes[ix][0]);
      glEnd;
    end;
  end;

begin
  if poly <> nil then
  begin

    OpenGLControl1Resize(nil);

    glClearColor(0, 0, 0, 1);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;

    glEnable(GL_DEPTH_TEST);
    //sceneInit;

    glTranslatef(0, 0, scale);
    glRotatef(anglex, 1, 0, 0);
    glRotatef(-angley, 0, 1, 0);

    drawFaces;
    if length(poly.faces) < 500 then
      drawLines;

    OpenGLControl1.SwapBuffers;
  end;

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

procedure TForm1.polyDefEditingDone(Sender: TObject);
begin
  genPoly;
end;

procedure TForm1.genPoly;

begin
  polyDef.Text := uppercase(polyDef.Text);

  poly.Free; // must free before new creation
  poly := polyhedron(polyDef.Text);

  t0 := now;

  transformPoly(transform.Text, poly);

  lap := MilliSecondsBetween(now, t0);

  poly.scaleVertexes;
  poly.calcAll;

  dispStat;

  OpenGLControl1.Invalidate;
end;

procedure TForm1.dispStat;
begin
  Caption := poly.Name;
  StatusBar1.SimpleText := Format('%0d faces, %1d vertexes, lap:%2dms',
    [length(poly.faces), length(poly.vertexes), lap]);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  scale := -4;
  genPoly;
end;

procedure TForm1.randomPolyClick(Sender: TObject);
begin
  polyDef.Text := uppercase(polyDef.Text);
  poly.Free; // must free before new creation
  poly := polyhedron(polyDef.Text);

  t0 := now;

  randTrans(poly);

  lap := MilliSecondsBetween(now, t0);

  poly.scaleVertexes;
  poly.calcAll;

  dispStat;

  OpenGLControl1.Invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  poly.Free;
end;

procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft: down := True;
    mbRight:
    begin
      poly.reColor;
      OpenGLControl1.Invalidate;
    end;
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
