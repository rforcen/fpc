unit ubaseApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, gl, glCommon;

type

  { TForm1 }

  TForm1 = class(TForm)
    IdleTimer1: TIdleTimer;
    glPanel: TPanel;
    sb: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
  private

  end;

  { TradGL }

  TradGL = class(TGL)
  public
    destructor Destroy; override;
    procedure draw; override;
    procedure init; override;
  private
    txsPeople, txsMandalas: TTextures;
    TxEarth: TTexture;
  end;

var
  Form1: TForm1;

  radGL: TradGL;
  wtm, sphEarth: TPolyhedron;
  tubedDodeca: TTube;

implementation

{$R *.lfm}


{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  radGL := TradGL.Create(glPanel, True);

  sphEarth := sphere(1, 30);
  wtm := waterman(34);
  tubedDodeca := TTube.Create(trackPoly(Dodecahedron), 0.02, 20, mkVertex(0.5, 0.5, 0));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  radGL.Free;
end;

procedure TForm1.IdleTimer1Timer(Sender: TObject);
begin
  radGL.incAngles(0, 0.2);
end;


{ TradGL }

destructor TradGL.Destroy;
begin
  if radGL.MakeCurrent then // when gl resources are used
  begin
    //releaseTextures(txsPeople);
    //releaseTextures(txsMandalas);
    TxEarth.Delete;
    tubedDodeca.Free;
  end;



  inherited Destroy;
end;

procedure TradGL.draw;
begin

  //drawPolySolid(sphEarth, txEarth);
  //tubedDodeca.draw;


  glDisable(GL_TEXTURE_2D);
  glPointSize(5);
  drawPolyPoints(wtm);

  //glScalef(0.2, 0.2, 0.2);
  //chrysanthemum;

  //drawPolySolid(Dodecahedron, txsPeople);

  //identity(180, -45);
  //drawPolyFaces(Cube, [0, 1, 2], txsMandalas);     // no need to swapbuffers here

end;

procedure TradGL.init;
begin     // must be called in  a gl context owned environment
  sceneInit;

  //  txsPeople := loadTexturesFromPath('D:\Pictures\people\', 5);
  //  txsMandalas := loadTexturesFromPath(
  //    'D:\important\development\vsw8\QuantumRadionics\backgrounds\Mandalas\');

  txEarth.load('C:\Users\rober\OneDrive\Imágenes\earthday.jpg');

end;

end.
