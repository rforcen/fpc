unit glRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, OpenGLContext, gl, Types;

type
  TGLRenderer = class(TOpenGLControl)
    procedure OpenGLControl1MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, Y: integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);



  public
    anglex, angley, scale: single;
    lastx, lasty: integer;
    down: boolean;
  end;

implementation

procedure TGLRenderer.OpenGLControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  down := True;
end;

procedure TGLRenderer.OpenGLControl1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
const
  d = 2.0;
begin
  if down then
  begin

    angley := angley + (x - lastx) / d;
    anglex := anglex + (y - lasty) / d;

    lastx := X;
    lasty := Y;

    Invalidate;
  end;
end;

procedure TGLRenderer.OpenGLControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  down := False;
end;

procedure TGLRenderer.OpenGLControl1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  scale := scale + WheelDelta / 4000.0;
  Invalidate;
end;

end.
