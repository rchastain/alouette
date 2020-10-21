
uses
  SysUtils, Board;

const
  CComma: array[boolean] of string = ('', ',');
  
var
  t: text;
  x, y: integer;
  LBoard: TBoard;
  
begin  
  Assign(t, 'column.pas');
  Rewrite(t);
  for x := 0 to 7 do
  begin
    LBoard := 0;
    for y := 0 to 7 do
      SwitchOn(LBoard, ToBoard(x, y));
    Write(t, ' $', IntToHex(LBoard, 16));
    WriteLn(t, CComma[x < 7]);
  end;
  Close(t);
end.
