unit logSpiral;

{$mode ObjFPC}{$H+}

interface

uses Graphics;

procedure draw(cnv: TCanvas; a: single; turns: integer; deltaTH: single);


implementation

uses
  Classes, SysUtils, musicFreq, Math;

procedure draw(cnv: TCanvas; a: single; turns: integer; deltaTH: single);
const
  MIN_RADIO = 10;
var
  x0, y0, th, r, x, y: single;
  ft: boolean;
  w, h: integer;

begin

  w := cnv.Width;
  h := cnv.Height;

  x := 0;
  y := 0;
  r := 0;

  x0 := w / 2 + x;
  y0 := h / 2 + y;
  ft := True;


  if (turns <> 0) and (deltaTH <> 0) and (a <> 0) then
  begin

    th := 0;
    while (th < PI * 2 * turns) and (r < w) do
    begin

      r := power(a, th);
      x := r * sin(th);
      y := r * cos(th);


      if r > MIN_RADIO then  // inner minimum radio
      begin
        if ft then
        begin
          cnv.MoveTo(round(x + x0), round(y + y0));
          ft := False;
        end
        else
        begin
          cnv.Pen.Color := Freq2Color(th);
          cnv.LineTo(round(x + x0), round(y + y0));
        end;

      end;

      th := th + deltaTH;
    end;
  end;
end;

end.
