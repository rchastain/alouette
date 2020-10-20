
{**
  @abstract(Numérotation des positions de départ aux échecs de Fischer.)
  Fonction pour la numérotation des positions de départ aux échecs de Fischer, par la méthode de Reinhard Scharnagl.
}

unit Number;

interface

function PositionNumber(const APos: string): integer;

implementation

function Row1(const APos: string): string;
var
  i: integer;
begin
  result := APos;
  repeat
    i := Pos('/', result);
    if i > 0 then
      Delete(result, 1, i);
  until i = 0;
  i := Pos(' ', result);
  if i > 0 then
    result := Copy(result, 1, Pred(i));
end;

function PositionNumber(const APos: string): integer;
const
  CNotFound = -1;
var
  LWhiteSquareBishopPos: CNotFound..3;
  LBlackSquareBishopPos: CNotFound..3;
  LQueenPos: CNotFound..5;
  LKRNCode: 0..9;
  i, x: integer;
  s, t: string; 
begin
  s := Row1(APos);
  Assert(Length(s) = 8);
  
  LWhiteSquareBishopPos := CNotFound;
  LBlackSquareBishopPos := CNotFound;
  LQueenPos := CNotFound;
  
  i := 0;
  while (i < 4) and (LWhiteSquareBishopPos = CNotFound) do
    if s[2 * i + 2] = 'B' then
      LWhiteSquareBishopPos := i
    else
      Inc(i);
  
  i := 0;
  while (i < 4) and (LBlackSquareBishopPos = CNotFound) do
    if s[2 * i + 1] = 'B' then
      LBlackSquareBishopPos := i
    else
      Inc(i);
  
  Assert(LWhiteSquareBishopPos > CNotFound);
  Assert(LBlackSquareBishopPos > CNotFound);
  
  i := 0;
  x := 1;
  while (x < 9) and (LQueenPos = CNotFound) do
  begin
    if s[x] = 'Q' then
      LQueenPos := i
    else
    begin
      Inc(x);
      if s[x] <> 'B' then
        Inc(i);
    end;
  end;
  
  t := '';
  for x := 1 to 8 do
    if s[x] in ['N', 'R', 'K'] then
      t := t + s[x];
  
  Assert(Length(t) = 5);
  
  if t = 'NNRKR' then LKRNCode := 0 else
  if t = 'NRNKR' then LKRNCode := 1 else
  if t = 'NRKNR' then LKRNCode := 2 else
  if t = 'NRKRN' then LKRNCode := 3 else
  if t = 'RNNKR' then LKRNCode := 4 else
  if t = 'RNKNR' then LKRNCode := 5 else
  if t = 'RNKRN' then LKRNCode := 6 else
  if t = 'RKNNR' then LKRNCode := 7 else
  if t = 'RKNRN' then LKRNCode := 8 else
  if t = 'RKRNN' then LKRNCode := 9 else
    Assert(FALSE);
  
  result := LWhiteSquareBishopPos + 4 * LBlackSquareBishopPos + 16 * LQueenPos + 96 * LKRNCode;
end;

end.
