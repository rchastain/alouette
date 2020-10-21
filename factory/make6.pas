
uses
  SysUtils, Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  t: text;
  x, y: integer;
  
begin  
  Assign(t, 'coordinates.pas');
  Rewrite(t);
  for x := 0 to 7 do
  begin
    WriteLn(t, '(');
    for y := 0 to 7 do
    begin
      Write(t, ' $', IntToHex(ToBoard(x, y), 16));
      WriteLn(t, CComma[y < 7]);
    end;
    WriteLn(t, ')', CComma[x < 7]);
  end;
  Close(t);
end.
