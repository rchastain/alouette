
uses
  Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  i: integer;
  t: text;
  
begin
  Assign(t, 'index.inc');
  Rewrite(t);
  for i := 0 to 63 do
  begin
    Write(t, ToBoard(i));
    WriteLn(t, CComma[i < 63]);
  end;
  Close(t);
end.
