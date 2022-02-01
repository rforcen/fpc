program mandelbrot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}

  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, uMandelbrot;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='mandelbrot';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

