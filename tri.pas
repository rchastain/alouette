
{**
  @abstract(Tri des coups en fonction de leur note.)
  Procédure pour trier les coups en fonction de leur note.
}

unit Tri;

interface

procedure Trie(var ACoup, ANote: array of integer; const n: integer);

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

end.
