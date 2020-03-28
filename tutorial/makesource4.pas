
uses
  SysUtils, Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  LSqr1, LSqr2: integer;
  t: text;
  i: int64;
  
begin
  Assign(t, 'path.inc');
  Rewrite(t);
  for LSqr1 := 0 to 63 do
  begin
    WriteLn(t, '(');
    for LSqr2 := 0 to 63 do
    begin
      i := GetPath(LSqr1, LSqr2);
      Write(t, '$', IntToHex(i, 16));
      WriteLn(t, CComma[LSqr2 < 63]);
    end;
    WriteLn(t, ')', CComma[LSqr1 < 63]);
  end;
  Close(t);
end.
