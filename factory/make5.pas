
uses
  SysUtils, Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  t: text;
  i: integer;
  
begin
  Assign(t, 'index.pas');
  Rewrite(t);
  for i := 0 to 63 do
  begin
    Write(t, ' $', IntToHex(ToBoard(i), 16));
    Write(t, CComma[i < 63]);
    if Succ(i) mod 4 = 0 then
      WriteLn(t);
  end;
  Close(t);
end.
