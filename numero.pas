
{ Numérotation des positions de départ aux échecs de Fischer, par la méthode de Reinhard Scharnagl. }
unit Numero;

interface

function NumeroPosition(const ABlanches: string): integer;

implementation

function NumeroPosition(const ABlanches: string): integer;
const
  CNotFound = -1;
var
  whiteSquareBishopPos: CNotFound..3;
  blackSquareBishopPos: CNotFound..3;
  queenPos: CNotFound..5;
  krnCode: 0..9;
  i, x: integer;
  s: string; 
begin
  Assert(Length(ABlanches) = 8);
  
  whiteSquareBishopPos := CNotFound;
  blackSquareBishopPos := CNotFound;
  queenPos := CNotFound;
  
  i := 0;
  while (i < 4) and (whiteSquareBishopPos = CNotFound) do
    if ABlanches[2 * i + 2] = 'B' then
      whiteSquareBishopPos := i
    else
      Inc(i);
  
  i := 0;
  while (i < 4) and (blackSquareBishopPos = CNotFound) do
    if ABlanches[2 * i + 1] = 'B' then
      blackSquareBishopPos := i
    else
      Inc(i);
  
  Assert(whiteSquareBishopPos > CNotFound);
  Assert(blackSquareBishopPos > CNotFound);
  
  i := 0;
  x := 1;
  while (x < 9) and (queenPos = CNotFound) do
  begin
    if ABlanches[x] = 'Q' then
      queenPos := i
    else
    begin
      Inc(x);
      if ABlanches[x] <> 'B' then
        Inc(i);
    end;
  end;
  
  s := '';
  for x := 1 to 8 do
    if ABlanches[x] in ['N', 'R', 'K'] then
      s := s + ABlanches[x];
  
  Assert(Length(s) = 5);
  
  if s = 'NNRKR' then krnCode := 0 else
  if s = 'NRNKR' then krnCode := 1 else
  if s = 'NRKNR' then krnCode := 2 else
  if s = 'NRKRN' then krnCode := 3 else
  if s = 'RNNKR' then krnCode := 4 else
  if s = 'RNKNR' then krnCode := 5 else
  if s = 'RNKRN' then krnCode := 6 else
  if s = 'RKNNR' then krnCode := 7 else
  if s = 'RKNRN' then krnCode := 8 else
  if s = 'RKRNN' then krnCode := 9 else
    Assert(FALSE);
  
  result := whiteSquareBishopPos + 4 * blackSquareBishopPos + 16 * queenPos + 96 * krnCode;
end;

end.
