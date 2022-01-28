// array of uint32 to image conversion
// and jpg writter

{$mode objfpc}{$h+}
unit u32toimage;
 
interface

uses classes, sysutils,
     FPImage, FPWriteJPEG;

function u32ToImage(au32:array of uint32; w,h:integer) : TFPMemoryImage;
procedure u32ToJPEG(au32:array of uint32; w,h:integer; fileName:string);

implementation

// 
function u32ToImage(au32:array of uint32; w,h:integer) : TFPMemoryImage;
var 
  i,j : integer;
  pix : uint32;
  col : TFPColor; // rgba in 0..$ffff range -> (0..$ff) << 8
begin
  u32ToImage := TFPMemoryImage.Create(w,h);

  for i:=0 to w-1 do
    for j:=0 to h-1 do begin
      pix:=au32[i*w + j];
      with col do begin
        red:=(pix and $FF) shl 8; 
        green:=((pix shr 8) and $FF) shl 8; 
        blue:=((pix shr 16) and $FF) shl 8; 
        alpha:=((pix shr 24) and $FF) shl 8;
      end;
      u32ToImage.Colors[i, j] := col;
    end;
end;

//
procedure u32ToJPEG(au32:array of uint32; w,h:integer; fileName:string);
var  
  writer: TFPCustomImageWriter;
  image : TFPMemoryImage;

begin
  image:=u32ToImage(au32, w,h);

  writer:=TFPWriterJPEG.Create;
  image.SaveToFile(fileName, writer);

  image.free;
  writer.free;
end; 

end.