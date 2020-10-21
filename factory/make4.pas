
uses
  SysUtils, Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  t: text;
  LFrom, LTo: integer;
  LBoard: int64;
  
begin
  Assign(t, 'path.pas');
  Rewrite(t);
  for LFrom := 0 to 63 do
  begin
    WriteLn(t, '(');
    for LTo := 0 to 63 do
    begin
      LBoard := GetPath(LFrom, LTo);
      Write(t, ' $', IntToHex(LBoard, 16));
      Write(t, CComma[LTo < 63]);
      if Succ(LTo) mod 4 = 0 then
        WriteLn(t);
    end;
    WriteLn(t, ')', CComma[LFrom < 63]);
  end;
  Close(t);
end.
