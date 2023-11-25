unit uSpiral;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Types, logSpiral;

type

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Panel2: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PaintBox1Click(Sender: TObject);
    procedure PaintBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure PaintBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure PaintBox1Paint(Sender: TObject);

    procedure DoAction(Sender: TObject; var Done: boolean);
  private

  public

  end;


const
  INC_DELTA = 0.00005;
  A_INIT = 1.01;
  TURNS = 300;
  INC_DELTA_ANIM = 0.001;

var
  Form1: TForm1;
  deltaTH: double;
  animate: boolean;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DoAction(Sender: TObject; var Done: boolean);
begin
  if animate then
  begin
    deltaTH += INC_DELTA;
    Invalidate;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  with Paintbox1 do
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);

    logSpiral.draw(Canvas, A_INIT, TURNS, deltaTH);
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  animate := False;
  deltaTH := 1;

  Application.OnIdle := @DoAction;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  animate := False;
end;


procedure TForm1.PaintBox1Click(Sender: TObject);
begin
  animate := not animate;
end;

procedure TForm1.PaintBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  deltaTH := deltaTH - INC_DELTA_ANIM;
  Invalidate;
end;

procedure TForm1.PaintBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  deltaTH := deltaTH + INC_DELTA_ANIM;
  Invalidate;
end;


end.
