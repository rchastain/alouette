
uses
  SysUtils, Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  t: text;
  x, y: integer;
  LType: TPieceType;
  LBoard: int64;
  
begin
  Assign(t, 'targets.pas');
  Rewrite(t);
  for LType in TPieceType do
  begin
    WriteLn(t, '(');
    for y := 0 to 7 do
      for x := 0 to 7 do
      begin
        LBoard := GetTargets(LType, 8 * y + x);
        Write(t, ' $', IntToHex(LBoard, 16));
        Write(t, CComma[8 * y + x < 63]);
        if (x = 7) or (x = 3) then
          WriteLn(t);
      end;
    WriteLn(t, ')', CComma[LType < ptKing]);
  end;
  Close(t);
end.
