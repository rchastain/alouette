
{**
  @abstract(Génération chronométrée.)
  Génération chronométrée de tous les coups jusqu'à une profondeur donnée.
}

unit Perft;

interface

uses
  Chess;

procedure Start(const APos: TPosition; const ADepth: integer = 5);

implementation

uses
  SysUtils, Move, Moves, Castling, Board, Tables, Sort;

function Evaluate(const APos: TPosition; const AMove: integer): integer;
var
  LPos: TPosition;
begin
  LPos := APos;
  result := Low(integer);
  if not FRejoue(LPos, NomCoup(AMove)) then
    Exit;
  result := 0 - Ord((FCoups(LPos) and LPos.Pieces[not LPos.Trait] and LPos.Rois) <> 0);
end;

function GetMovesCount(const APos: TPosition; const AProf: integer): int64;
var
  LList, LValue: array[0..99] of integer;
  n, o, i: integer;
  LPos: TPosition;
begin
  result := 0;
  FCoups(APos, LList, n);
  FRoque(APos, LList, n);
  for i := 0 to Pred(n) do
    LValue[i] := Evaluate(APos, LList[i]);
  Trie(LList, LValue, n);
  o := 0;
  while (o < n) and (LValue[o] = 0) do
    Inc(o);
  if AProf = 1 then
    result := o
  else
    for i := 0 to Pred(o) do
    begin
      LPos := APos;
      if not FRejoue(LPos, NomCoup(LList[i])) then
      begin
        WriteLn('Il y a quelque chose de pourri dans ce programme.');
        Continue;
      end;
      Inc(result, GetMovesCount(LPos, Pred(AProf)));
    end;
end;

procedure Start(const APos: TPosition; const ADepth: integer);
var
  p: TPosition;
  i: integer;
  t: cardinal;
  n: int64;
begin
  p := APos;
  WriteLn('Profondeur   Nombre trouvé   Temps écoulé');
  for i := 1 to ADepth do
  begin
    t := GetTickCount64;
    n := GetMovesCount(p, i);
    t := GetTickCount64 - t;
    WriteLn(i:10, n:16, FormatDateTime('   hh:nn:ss:zzz', t / (1000 * SECSPERDAY)));
  end;
end;

end.
