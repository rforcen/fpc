{ custom control used to draw an image from a imagelist on a listbox }
unit uTCC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, LCLType, uImgHelper;

  { TCC - custom control derived }

type

  TCC = class(TCustomControl)
  public
    constructor Create(_parent: TWinControl; const _il: TImageList); overload;
    destructor Destroy; override;

    procedure drawImage(Control: TWinControl; Index: integer; ARect: TRect;
      State: TOwnerDrawState);

  private
    il: TImageList;
    imgTmp: TImage;
  end;

implementation

constructor TCC.Create(_parent: TWinControl; const _il: TImageList);
begin
  inherited Create(nil);

  //Parent := _parent;  { the listbox, not required as drawImage receives it }
  il := _il;  { image list w/all images }

  imgTmp := TImage.Create(nil);
end;

destructor TCC.Destroy;
begin
  imgTmp.Free;

  inherited Destroy;
end;


procedure TCC.drawImage(Control: TWinControl; Index: integer; ARect: TRect;
  State: TOwnerDrawState);
begin
  ARect.Inflate(-3, -3); // 3 px spacing

  with (Control as TListBox).Canvas do // pant is listbox, draw in it
  begin
    { draw the image }
    il.GetBitmap(Index, imgTmp.picture.bitmap); { get imgTmp }
    StretchDraw(ARect, imgTmp.Picture.Graphic); { draw stretch }

    if odFocused in State then { red rect if required }
    begin
      brush.Style := bsClear;
      pen.Color := clRed;
      pen.Width := 3;
      rectangle(ARect);
    end;
  end;
end;

end.
