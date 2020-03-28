
uses
  Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  b: TBoard;
  i, j: integer;
  t: text;
  
begin  
  Assign(t, 'column.inc');
  Rewrite(t);
  for i := 0 to 7 do
  begin
    b := 0;
    for j := 0 to 7 do
      SwitchOn(b, ToBoard(i, j));
    Write(t, b);
    WriteLn(t, CComma[i < 7]);
  end;
  Close(t);
end.
