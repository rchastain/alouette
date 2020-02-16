
{**
  @abstract(Numérotation des positions de départ aux échecs de Fischer.)
  Fonction pour la numérotation des positions de départ aux échecs de Fischer, par la méthode de Reinhard Scharnagl.
}

unit Number;

interface

function NumeroPosition(const ABlanches: string): integer;

implementation

function LigneUn(const AChaine: string): string;
var
  i: integer;
begin
  result := AChaine;
  repeat
    i := Pos('/', result);
    if i > 0 then
      Delete(result, 1, i);
  until i = 0;
  i := Pos(' ', result);
  if i > 0 then
    result := Copy(result, 1, Pred(i));
end;

function NumeroPosition(const ABlanches: string): integer;
const
  CNotFound = -1;
var
  whiteSquareBishopPos: CNotFound..3;
  blackSquareBishopPos: CNotFound..3;
  queenPos: CNotFound..5;
  krnCode: 0..9;
  i, x: integer;
  s, t: string; 
begin
  s := LigneUn(ABlanches);
  Assert(Length(s) = 8);
  
  whiteSquareBishopPos := CNotFound;
  blackSquareBishopPos := CNotFound;
  queenPos := CNotFound;
  
  i := 0;
  while (i < 4) and (whiteSquareBishopPos = CNotFound) do
    if s[2 * i + 2] = 'B' then
      whiteSquareBishopPos := i
    else
      Inc(i);
  
  i := 0;
  while (i < 4) and (blackSquareBishopPos = CNotFound) do
    if s[2 * i + 1] = 'B' then
      blackSquareBishopPos := i
    else
      Inc(i);
  
  Assert(whiteSquareBishopPos > CNotFound);
  Assert(blackSquareBishopPos > CNotFound);
  
  i := 0;
  x := 1;
  while (x < 9) and (queenPos = CNotFound) do
  begin
    if s[x] = 'Q' then
      queenPos := i
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
  
  if t = 'NNRKR' then krnCode := 0 else
  if t = 'NRNKR' then krnCode := 1 else
  if t = 'NRKNR' then krnCode := 2 else
  if t = 'NRKRN' then krnCode := 3 else
  if t = 'RNNKR' then krnCode := 4 else
  if t = 'RNKNR' then krnCode := 5 else
  if t = 'RNKRN' then krnCode := 6 else
  if t = 'RKNNR' then krnCode := 7 else
  if t = 'RKNRN' then krnCode := 8 else
  if t = 'RKRNN' then krnCode := 9 else
    Assert(FALSE);
  
  result := whiteSquareBishopPos + 4 * blackSquareBishopPos + 16 * queenPos + 96 * krnCode;
end;

end.
