unit uGLRender;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, gl, Controls;

type
  GLRender = class(TOpenGLControl)
  public
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControlMouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, Y: integer);
    procedure OpenGLControlMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);

  public
    scale: single;

  private
    anglex, angley: single;
    lastx, lasty: integer;
    down: boolean;
  end;

implementation

procedure GLRender.OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  case Button of
    mbLeft: down := True;
    mbRight:
    begin
      Invalidate;
    end;
    else;
  end;
end;

procedure GLRender.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
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

    Invalidate;
  end;
end;

procedure GLRender.OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  down := False;
end;

procedure GLRender.OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  scale := scale + WheelDelta / 1000;

  Invalidate;
end;

end.
