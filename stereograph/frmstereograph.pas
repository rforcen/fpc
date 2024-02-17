unit frmStereograph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, uStereoGraph, Types, LCLType, ColorBox, Menus, uImgHelper;

type

  TImageType = (itDepthMap, itTexture);
  { TfrmStereo }

  TfrmStereo = class(TForm)
    cb1: TColorListBox;
    gbDepthMap: TGroupBox;
    gbTextures: TGroupBox;
    ilDepthMap: TImageList;
    ilTextures: TImageList;
    img: TImage;
    lbDepthMap: TListBox;
    lbTextures: TListBox;
    mapPanel: TPanel;
    imgMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    save2k: TMenuItem;
    save4k: TMenuItem;
    save8k: TMenuItem;
    procedure cb1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgClick(Sender: TObject);
    procedure listBoxClick(Sender: TObject);
    procedure listBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure saveClick(Sender: TObject);
  private
    procedure drawDepthMap;
    procedure drawTextureMap;
    procedure populate(const lb: TListBox; const il: TImageList);
    procedure display(it: TImageType);
  public
    strgm, depthMap, texture: TImage;
    imgType: TImageType;
    colorDepth: integer;

  end;

var
  frmStereo: TfrmStereo;

implementation

{$R *.lfm}


{ TfrmStereo }

procedure TfrmStereo.FormCreate(Sender: TObject);
begin

  texture := TImage.Create(nil); { create images }
  depthMap := TImage.Create(nil);

  populate(lbTextures, ilTextures); { populate lb w/il}
  populate(lbDepthMap, ilDepthMap);

  randomize; { display random map }

  imgType := itDepthMap;
  lbDepthMap.ItemIndex := random(pred(ilDepthMap.Count));
  lbTextures.ItemIndex := 0;
  colorDepth := clRed;
end;

procedure TfrmStereo.cb1Click(Sender: TObject);
begin
  colorDepth := cb1.Selected;
  drawDepthMap;
end;

procedure TfrmStereo.FormDestroy(Sender: TObject);
begin
  depthMap.Free;  { free used images & custom controls objects used in list draw }
  texture.Free;
end;

procedure TfrmStereo.FormResize(Sender: TObject);
begin
  if imgType = itDepthMap then drawDepthMap
  else
    drawTextureMap;
end;

procedure TfrmStereo.imgClick(Sender: TObject);
const
  ws: array[boolean] of TWindowState = (wsNormal, wsFullScreen);
begin
  frmStereo.WindowState := ws[mapPanel.Visible];
  mapPanel.Visible := not mapPanel.Visible;
end;

procedure TfrmStereo.listBoxClick(Sender: TObject);
begin
  if (Sender as TListBox) = lbDepthMap then
    display(itDepthMap)
  else
  if (Sender as TListBox) = lbTextures then
    display(itTexture);
end;

procedure TfrmStereo.listBoxDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  { draw image from xxImageList to lbListBox }

  { imagelist is stores in the object part of items 'populate' }
  ((Control as TListBox).Items.Objects[Index] as TImageList)
  .GetBitmap(Index, depthMap.picture.bitmap);


  with (Control as TListBox).Canvas do { selected listbox }
  begin
    ARect.Inflate(-3, -3);  { 3 px spacing }
    StretchDraw(ARect, depthMap.Picture.Graphic); { draw stretch }
    if odFocused in State then { red rect if required }
    begin
      brush.Style := bsClear;
      pen.Color := clRed;
      pen.Width := 3;
      rectangle(ARect);
    end;
  end;
end;

procedure TfrmStereo.saveClick(Sender: TObject);

  procedure savenk(k: integer);
  begin
    case imgType of // from latest generated img
      itDepthmap {dm}: strgm :=
          generateSIRD(depthMap, clblack, clwhite, colorDepth, 0.5, k * 1024, k * 1024);
      itTexture {tex}: strgm :=
          generateTexturedSIRD(depthMap, texture, k * 1024, k * 1024);
      else;
    end;

    strgm.Picture.SaveToFile(format('stereogram_%dk.jpg', [k]));
    strgm.Free;
  end;

begin
  case (Sender as TMenuItem).Name of
    'save2k': savenk(2);
    'save4k': savenk(4);
    'save8k': savenk(8);
    else;
  end;
end;

procedure TfrmStereo.drawDepthMap;
begin
  ilDepthMap.GetBitmap(lbDepthMap.ItemIndex, depthMap.picture.bitmap);

  strgm := generateSIRD(depthMap, clblack, clwhite, colorDepth, 0.5,
    img.Width, img.Height);

  img.picture.Assign(strgm.picture);
  strgm.Free;
end;

procedure TfrmStereo.drawTextureMap;
begin
  { load selected depthMap }
  ilTextures.GetBitmap(lbTextures.ItemIndex, texture.picture.bitmap);
  if lbDepthMap.ItemIndex < 0 then lbDepthMap.ItemIndex := 0;

  { & texture map }
  ilDepthMap.GetBitmap(lbDepthMap.ItemIndex, depthMap.picture.bitmap);

  strgm := generateTexturedSIRD(depthMap, texture, img.Width, img.Height);
  img.picture.Assign(strgm.picture);
  strgm.Free;
end;

procedure TfrmStereo.populate(const lb: TListBox; const il: TImageList);
var
  i: integer;
begin
  for i := 0 to pred(il.Count) do { populate listbox w/image list as associated obejct}
    lb.AddItem('', il);
end;

procedure TfrmStereo.display(it: TImageType);
begin
  imgType := it;
  case it of
    itDepthMap: drawDepthMap;
    itTexture: drawTextureMap;
    else;
  end;
end;


end.
