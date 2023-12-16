program dc;

{$mode objfpc}{$H+}

uses
  {$ifdef linux}
  //cthreads,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, domainColoring, zCompiler, assignImage;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

