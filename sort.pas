
{**
  @abstract(Tri des coups en fonction de leur note.)
  Procédure pour trier les coups en fonction de leur note.
}

unit Sort;

interface

procedure SortMoves(var AMoves, AValues: array of integer; const n: integer);

implementation

procedure Swap(var AArray: array of integer; const i: integer);
var
  j: integer;
begin
  j := AArray[i];
  AArray[i] := AArray[i + 1];
  AArray[i + 1] := j;
end;

procedure SortMoves(var AMoves, AValues: array of integer; const n: integer);
var
  LIdx: integer;
  LDone: boolean;
begin
  repeat
    LDone := TRUE;
    for LIdx := 0 to n - 2 do
      if AValues[LIdx] < AValues[LIdx + 1] then
      begin
        Swap(AMoves, LIdx);
        Swap(AValues, LIdx);
        LDone := FALSE;
      end;
  until LDone;
end;

end.
