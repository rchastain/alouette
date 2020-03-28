
uses
  SysUtils, Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  x, y: integer;
  p: TPieceType;
  t: text;
  i: int64;
  
begin
  Assign(t, 'targets.inc');
  Rewrite(t);
  for p in TPieceType do
  begin
    WriteLn(t, '(');
    for y := 0 to 7 do
      for x := 0 to 7 do
      begin
        i := GetTargets(p, 8 * y + x);
        Write(t, '$', IntToHex(i, 16));
        WriteLn(t, CComma[8 * y + x < 63]);
      end;
    WriteLn(t, ')', CComma[p < ptKing]);
  end;
  Close(t);
end.
