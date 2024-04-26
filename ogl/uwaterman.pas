unit uWaterman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  StdCtrls, ComCtrls, iWaterman, gl, glCommon;

type

  { TfrmWaterman }

  TfrmWaterman = class(TForm)
    btnReColor: TButton;
    cbDrawMode: TComboBox;
    imgTexture: TImage;
    panelTop: TPanel;
    panelGL: TPanel;
    seRad: TSpinEdit;
    sb: TStatusBar;
    procedure btnReColorClick(Sender: TObject);
    procedure cbDrawModeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure seRadChange(Sender: TObject);
  private
  public
  end;

  TDrawMode = (Wired, Tubed, FacedColor, Textured);

  { TWatGL }

  TWatGL = class(TGL)
  public
    constructor Create(panel: TWinControl; _rad: integer; _pic: TPicture);
    destructor Destroy; override;
    procedure draw; override;
    procedure init; override;

    procedure setRad(rad: integer);
    procedure reDraw;
    procedure createTubes;
  private
    w, wp: TPolyhedron;
    drawMode: TDrawMode;
    tubes: TTube;
    fractText: TTexture;
    rad: integer;
    pic: TPicture;
  end;



var
  frmWaterman: TfrmWaterman;
  glWat: TWatGL;

implementation

{$R *.lfm}

{ TfrmWaterman }

procedure TfrmWaterman.FormCreate(Sender: TObject);
begin
  glWat := TWatGL.Create(panelGL, seRad.Value, imgTexture.Picture);
end;

procedure TfrmWaterman.FormDestroy(Sender: TObject);
begin
  glWat.Free;
end;

procedure TfrmWaterman.btnReColorClick(Sender: TObject);
begin
  glWat.reDraw;
end;

procedure TfrmWaterman.cbDrawModeChange(Sender: TObject);
begin
  glWat.drawMode := TDrawMode(cbDrawMode.ItemIndex);

  if glWat.drawMode = Tubed then glWat.createTubes;

  glWat.reDraw;
end;

procedure TfrmWaterman.seRadChange(Sender: TObject);
begin
  glWat.setRad(seRad.Value);
  sb.SimpleText := format('vertexes:%d, faces:%d',
    [length(glWat.wp.vertexes), length(glWat.wp.faces)]);
end;

{ TWatGL }

constructor TWatGL.Create(panel: TWinControl; _rad: integer; _pic: TPicture);
begin
  inherited Create(panel);
  rad := _rad;
  pic := _pic;
  tubes := nil;
end;

destructor TWatGL.Destroy;
begin
  if tubes <> nil then tubes.Free;
  inherited Destroy;
end;

procedure TWatGL.draw;
begin
  fractText.disable;

  case drawMode of
    Wired: drawPolyLine(wp, mkVertex(1, 1, 1));

    Tubed: tubes.draw;

    FacedColor: begin
      drawPoly(wp, getStrobeColors(10));
      drawPolyLine(wp, mkVertex(0, 0, 0));
    end;

    Textured: begin
      drawPolySolid(wp, fractText);
      fractText.disable;
      drawPolyLine(wp, mkVertex(1, 1, 1));
    end;
  end;

end;

procedure TWatGL.init;
begin
  sceneInit;
  setRad(rad);
  glLineWidth(4);
  fractText.load(pic); // textures loaded here!
end;

procedure TWatGL.setRad(rad: integer);
begin
  w := waterman(rad);

  wp := quickHull3d(w.vertexes);

  newList;

  if drawMode = Tubed then createTubes;
  invalidate;
end;

procedure TWatGL.reDraw;
begin
  newList;
  invalidate;
end;

procedure TWatGL.createTubes;
begin
  if tubes <> nil then tubes.Free;
  tubes := TTube.Create(trackPoly(wp), 0.015, 15, mkVertex(0.5, 0.5, 0));
end;

end.
