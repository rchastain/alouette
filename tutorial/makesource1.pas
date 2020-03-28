
uses
  SysUtils;
  
function SquareName(const x, y: integer): string;
begin
  result := Concat(
    Chr(x + Ord('A')),
    Chr(y + Ord('1'))
  );
end;

var
  x, y: integer;
  t: text;
  
begin
  Assign(t, '01.pas');
  Rewrite(t);
  for y := 0 to 7 do
  begin
    Write(t, ' ');
    for x := 0 to 7 do
      Write(t, Format(' %s = %0.2d;', [SquareName(x, y), 8 * y + x]));
    WriteLn(t, '');
  end;
  Close(t);
end.
