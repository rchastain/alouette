
uses
  SysUtils;
  
function SquareName(const i: integer): string;
begin
  result := Concat(
    Chr(i mod 8 + Ord('A')),
    Chr(i div 8 + Ord('1'))
  );
end;

var
  i: integer;
  t: text;
  
begin
  Assign(t, '02.pas');
  Rewrite(t);
  for i := 0 to 63 do
  begin
    if i mod 8 <> 0 then
      Write(t, ' ');
    Write(t, '''', SquareName(i), '''');
    if i < 63 then
      Write(t, ',');
    if i mod 8 = 7 then
      WriteLn(t);
  end;
  Close(t);
end.
