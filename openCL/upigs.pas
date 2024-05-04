{
 PIGS : parallel image generation system

 openCL programs are contained in memo's in memosPanel
 each TOpenCL object share main oclV on except for program & kernel
 }
unit uPigs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, GraphType, uOpenCL, mesh, uImageDisp;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbTypes: TComboBox;
    Image1: TImage;
    mDC: TMemo;
    mSH: TMemo;
    mMandelbrot: TMemo;
    mVoronoi: TMemo;
    memosPanel: TPanel;
    topPanel: TPanel;
    sb: TStatusBar;
    procedure cbTypesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Resize(Sender: TObject);

  private
    procedure voronoiCL;
    procedure mandelbrotCL;
    procedure shCL;
    procedure domainColoringCL;

    procedure compileAll;
  public

  end;

  TArru32 = array of uint32;

var
  Form1: TForm1;
  oclMain, oclV, oclM, oclSH, oclDC: TOpenCL;
  t0: int64;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  oclMain.AMDdevice; // init device

  oclV := oclMain;
  oclM := oclMain;  // copy to all
  oclSH := oclMain;
  oclDC := oclMain;

  compileAll;
end;

procedure TForm1.cbTypesChange(Sender: TObject);
begin
  Image1Resize(Sender);
end;

procedure TForm1.compileAll;
begin
  oclV.compile(mVoronoi.Text, 'voronoi');
  oclM.compile(mMandelbrot.Text, 'mandelbrot');
  oclSH.compile(mSH.Text, 'spherical_harmonics');
  oclDC.compile(mDC.Text, 'domain_coloring');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  oclMain.Free;

  oclV.freePrg;
  oclM.freePrg;
  oclSH.freePrg;
  oclDC.freePrg;
end;

procedure TForm1.Image1Resize(Sender: TObject);
begin
  case cbTypes.ItemIndex of
    0: if oclV.ok then voronoiCL;
    1: if oclM.ok then mandelbrotCL;
    2: if oclSH.ok then shCL;
    3: if oclDC.ok then domainColoringCL;
    else;
  end;
end;

procedure TForm1.voronoiCL;
type
  TPoint = packed record
    x, y: single;
    color, pad: uint32;
  end;
var
  i, w, h, np, szImg, szPoints: integer;
  img: array of uint32 = nil;
  points: array of TPoint = nil;
  gpuImage, gpuPoints: TCLBuffer;
begin

  // create image & points
  w := Image1.Width;
  h := image1.Height;
  np := w * 4;

  setLength(img, w * h);
  setLength(points, np);

  szImg := length(img) * sizeof(img[0]);
  szPoints := length(points) * sizeof(points[0]);

  for i := 0 to pred(np) do
  begin
    with points[i] do
    begin
      x := random;
      y := random;
      color := random($00ffffff);
    end;
  end;

  // buffers
  gpuImage := oclV.buffer(szImg);
  gpuPoints := oclV.buffer(szPoints);

  oclV.setArgs([gpuImage, gpuPoints, np, w]);     // args
  oclV.Write(@points[0], gpuPoints, szPoints);    // write points

  t0 := gettickCount64;

  oclV.run(w * h);     // run
  oclV.Read(@img[0], gpuImage, szImg);     // read img

  t0 := gettickCount64 - t0;

  sb.SimpleText := format('voronoi %d x %d, #points:%d, lap:%d ms', [w, h, np, t0]);

  // release
  oclV.Release(gpuImage);
  oclV.Release(gpuPoints);

  // disp
  ImageDisp(image1, img);

end;

procedure TForm1.mandelbrotCL;
const
  center: array[0..1] of single = (2, 1);
  scale: single = 2;
  iters: int32 = 800;
var
  w, h, szImg: integer;
  img: array of uint32 = nil;
  gpuImage: TCLBuffer;
begin
  // create image array
  w := Image1.Width;
  h := image1.Height;
  setLength(img, w * h);
  szImg := length(img) * sizeof(img[0]);

  gpuImage := oclM.buffer(szImg);   // buffer

  // args, 8 byte float2 'center' is passed as a 8 byte pointer, sigle 'scale' is passed as int32
  oclM.setArgs([gpuImage, pointer(center), integer(scale), iters, w]);

  t0 := gettickCount64;
  oclM.run(w * h); // execute
  oclM.Read(@img[0], gpuImage, szImg); // read generated img
  t0 := gettickCount64 - t0;

  sb.SimpleText := format('mandelbrot %d x %d, %d iters, lap: %d ms', [w, h, iters, t0]);

  oclM.Release(gpuImage);     // release
  ImageDisp(image1, img);  // disp
end;

procedure TForm1.shCL;
const
  resol: int32 = 256 * 2;
  colorMap: int32 = 0;
  code: int32 = 190;
var
  mesh: TMeshes = nil;
  gpuMesh: TCLBuffer;
  meshBytes: int32;
begin
  meshBytes := resol * resol * sizeof(TMesh);
  setLength(mesh, resol * resol); // mesh buffer
  gpuMesh := oclSH.buffer(meshBytes);

  // args: gpuMesh, resol.int32, colorMap.int32, code.int32
  oclSH.setArgs([gpuMesh, resol, colorMap, code]);

  t0 := gettickCount64;
  oclSH.run(meshBytes); // execute
  oclSH.Read(@mesh[0], gpuMesh, meshBytes); // read generated mesh
  t0 := gettickCount64 - t0;

  sb.SimpleText := format('sh %d, generated file ''sh.wrl'' lap:%d ms', [resol, t0]);

  oclSH.Release(gpuMesh);     // release

  WriteWRL(mesh, generateFaces(resol), 'sh.wrl');
end;

procedure TForm1.domainColoringCL;
var
  w, h, szImg: integer;
  gpuImage: TCLBuffer;
  img: array of uint32 = nil;
begin
  w := Image1.Width;
  h := image1.Height;

  setLength(img, w * h);
  szImg := length(img) * sizeof(img[0]);

  gpuImage := oclDC.buffer(szImg);
  // .set args for: kernel void domain_coloring(global uint *image, int width)
  oclDC.setArgs([gpuImage, w]);

  t0 := gettickCount64;

  oclDC.run(w * h);                      // .run
  oclDC.Read(@img[0], gpuImage, szImg);  // .read img

  t0 := gettickCount64 - t0;

  sb.SimpleText := format('domain coloring %d x %d, lap:%d ms', [w, h, t0]);

  oclDC.Release(gpuImage);   // .release buffer

  ImageDisp(image1, img);  // disp
end;

end.
