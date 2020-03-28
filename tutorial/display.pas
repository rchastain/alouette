
unit Display;

interface

uses
  Board;

procedure DisplayBoard(const ABoard: TBoard);

implementation

procedure DisplayBoard(const ABoard: TBoard);
var
  x, y: integer;
  s: string;
begin
  s := BoardToStr(ABoard);
  WriteLn('+   abcdefgh   +'#13#10);
  for y := 7 downto 0 do
  begin
    Write(Succ(y), '   ');
    for x := 0 to 7 do
      Write(s[64 - 8 * y - x]);
    WriteLn('   ', Succ(y));
  end;
  WriteLn(#13#10'+   abcdefgh   +');
end;

end.
