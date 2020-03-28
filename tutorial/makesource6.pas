
uses
  Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  i, j: integer;
  t: text;
  
begin  
  Assign(t, 'coordinates.inc');
  Rewrite(t);
  for i := 0 to 7 do
  begin
    WriteLn(t, '(');
    for j := 0 to 7 do
    begin
      Write(t, ToBoard(i, j));
      WriteLn(t, CComma[j < 7]);
    end;
    WriteLn(t, ')', CComma[i < 7]);
  end;
  Close(t);
end.
