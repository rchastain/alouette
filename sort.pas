
{**
  @abstract(Tri des coups en fonction de leur note.)
  Procédure pour trier les coups en fonction de leur note.
}

unit Sort;

interface

procedure Trie(var ACoup, ANote: array of integer; const n: integer);
function Tronque(const AEval: array of integer; const n: integer; const ARepetition: integer = 0): integer;

implementation

procedure Echange(var ATableau: array of integer; const i: integer);
var
  j: integer;
begin
  j := ATableau[i];
  ATableau[i] := ATableau[i + 1];
  ATableau[i + 1] := j;
end;

procedure Trie(var ACoup, ANote: array of integer; const n: integer);
var
  LIdx: integer;
  LFin: boolean;
begin
  repeat
    LFin := TRUE;
    for LIdx := 0 to n - 2 do
      if ANote[LIdx] < ANote[LIdx + 1] then
      begin
        Echange(ACoup, LIdx);
        Echange(ANote, LIdx);
        LFin := FALSE;
      end;
  until LFin;
end;

function Tronque(const AEval: array of integer; const n: integer; const ARepetition: integer): integer;
var
  i, j, k: integer;
begin
  Assert(n > 1);
  i := Pred(n);
  j := AEval[i];
  i := Pred(i);
  k := ARepetition;
  while k >= 0 do
  begin
    while AEval[i] = j do
      if i > 0 then
        Dec(i)
      else
        Break;
    j := AEval[i];
    Dec(k);
  end;
  result := Succ(i);
end;

end.
