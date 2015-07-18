{this file is included in RichView.pas}
{------------------------------------}
procedure ReplaceStr(var str: String; old, new: String);
var p: Integer;
begin
   while true do begin
     p := pos(old, str);
     if p=0 then break;
     Delete(str,p, Length(old));
     Insert(new, str, p);
   end;
end;
{------------------------------------------------------------}
procedure ReplaceStr2(var str: String; old, new: String);
var p,ptr: Integer;
    s: String;
begin
   s := str;
   ptr := 1;
   while true do begin
     p := pos(old, s);
     if p=0 then break;
     inc(p, ptr-1);
     Delete(str,p, Length(old));
     Insert(new, str, p);
     ptr := p+Length(new);
     s := Copy(str, ptr, Length(str)+1-ptr);
   end;
end;
{------------------------------------------------------------}
function MakeHTMLStr(str:String): String;
begin
  ReplaceStr2(str, '&', '&amp');
  ReplaceStr(str, '>', '&gt');
  ReplaceStr(str, '<', '&lt');
  ReplaceStr(str, '  ', '&nbsp ');
  MakeHTMLStr := str;
end;

{------------------------------------------------------------}
function ColorCode(C: TColor): String;
var s: String;
begin
 s := IntToHex(ColorToRGB(c),6);
 s := Copy(s,5,2)+Copy(s,3,2)+Copy(s,1,2);
 ColorCode := '"#'+s+'"';
end;
{------------------------------------------------------------}
function GetFontSize(pts: Integer): Integer;
begin
  if pts<=8 then
    GetFontSize := 1
  else
    case pts of
      9..10:  GetFontSize := 2;
      11..12: GetFontSize := 3;
      13..14: GetFontSize := 4;
      15..18: GetFontSize := 5;
      19..24: GetFontSize := 6;
      else    GetFontSize := 7;
    end;
end;
{------------------------------------------------------------}
function OpenFontTag(ts: TFontInfo; normalfs: TFontStyles; Relative: Boolean): String;
var s: String;
begin
  s := '<FONT size='+IntToStr(GetFontSize(ts.Size))+ ' color='+ColorCode(ts.Color)+
       ' face="'+ts.FontName+'">';
  if Relative then begin
    if not (fsBold      in ts.Style) and (fsBold      in normalfs) then s := s+'</B>';
    if not (fsItalic    in ts.Style) and (fsItalic    in normalfs) then s := s+'</I>';
    if not (fsUnderline in ts.Style) and (fsUnderline in normalfs) then s := s+'</U>';
    if not (fsStrikeOut in ts.Style) and (fsStrikeOut in normalfs) then s := s+'</S>';
    if (fsBold      in ts.Style) and not (fsBold      in normalfs) then s := s+'<B>';
    if (fsItalic    in ts.Style) and not (fsItalic    in normalfs) then s := s+'<I>';
    if (fsUnderline in ts.Style) and not (fsUnderline in normalfs) then s := s+'<U>';
    if (fsStrikeOut in ts.Style) and not (fsStrikeOut in normalfs) then s := s+'<S>';
    end
  else begin
    if (fsBold in ts.Style)      then s := s+'<B>';
    if (fsItalic in ts.Style)    then s := s+'<I>';
    if (fsUnderline in ts.Style) then s := s+'<U>';
    if (fsStrikeOut in ts.Style) then s := s+'<S>';
  end;
  OpenFontTag := s;
end;
{------------------------------------------------------------}
function CloseFontTag(ts: TFontInfo; normalfs: TFontStyles; Relative: Boolean):String;
var s: String;
begin
  if Relative then begin
    if (fsBold      in ts.Style) and not (fsBold      in normalfs) then s := s+'</B>';
    if (fsItalic    in ts.Style) and not (fsItalic    in normalfs) then s := s+'</I>';
    if (fsUnderline in ts.Style) and not (fsUnderline in normalfs) then s := s+'</U>';
    if (fsStrikeOut in ts.Style) and not (fsStrikeOut in normalfs) then s := s+'</S>';
    if not (fsBold      in ts.Style) and (fsBold      in normalfs) then s := s+'<B>';
    if not (fsItalic    in ts.Style) and (fsItalic    in normalfs) then s := s+'<I>';
    if not (fsUnderline in ts.Style) and (fsUnderline in normalfs) then s := s+'<U>';
    if not (fsStrikeOut in ts.Style) and (fsStrikeOut in normalfs) then s := s+'<S>';
    end
  else begin
    if (fsBold in ts.Style)      then s := s+'</B>';
    if (fsItalic in ts.Style)    then s := s+'</I>';
    if (fsUnderline in ts.Style) then s := s+'</U>';
    if (fsStrikeOut in ts.Style) then s := s+'</S>';
  end;
  s:= s+'</FONT>';
  CloseFontTag := s;
end;
{------------------------------------------------------------}
function TRichView.GetNextFileName(Path: String): String;
var fn: String;
begin
  while True do begin
     inc(imgSaveNo);
     fn := Path+imgSavePrefix+IntToStr(imgSaveNo)+'.bmp';
     GetNextFileName := fn;
     if not FileExists(fn) then exit;
     if (rvsoOverrideImages in SaveOptions) and
        ((FileGetAttr(fn) and faReadOnly)=0) then exit;
  end;
end;
{------------------------------------------------------------}
function TRichView.SavePicture(DocumentSaveFormat: TRVSaveFormat; Path: String; gr: TGraphic): String;
var fn: String;
    bmp: TBitmap;
begin
  { DocumentSaveFormat in this version is ignored }
   fn := GetNextFileName(Path);
   SavePicture := ExtractFileName(fn);
   if gr is TBitmap then begin
     gr.SaveToFile(fn);
     exit;
   end;
   bmp := TBitmap.Create;
   try
     bmp.Height := gr.Height;
     bmp.Width := gr.Width;
     bmp.Canvas.Brush.Color := Style.Color;
     bmp.Canvas.Pen.Color := Style.Color;
     bmp.Canvas.FillRect(Rect(0,0,Width,Height));
     bmp.Canvas.Draw(0,0,gr);
     bmp.SaveToFile(fn);
   finally
     bmp.Free;
   end;

end;

