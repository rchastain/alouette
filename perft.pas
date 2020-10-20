
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
  if not TryDoMove(LPos, MoveToStr(AMove)) then
    Exit;
  result := 0 - Ord((GenMoves(LPos) and LPos.Pieces[not LPos.SideToMove] and LPos.Kings) <> 0);
end;

function RecursiveGetMovesCount(const APos: TPosition; const ADepth: integer): int64;
var
  LList, LValue: array[0..99] of integer;
  n, o, i: integer;
  LPos: TPosition;
begin
  result := 0;
  GenMoves(APos, LList, n);
  GenCastling(APos, LList, n);
  for i := 0 to Pred(n) do
    LValue[i] := Evaluate(APos, LList[i]);
  SortMoves(LList, LValue, n);
  o := 0;
  while (o < n) and (LValue[o] = 0) do
    Inc(o);
  if ADepth = 1 then
    result := o
  else
    for i := 0 to Pred(o) do
    begin
      LPos := APos;
      if not TryDoMove(LPos, MoveToStr(LList[i])) then
      begin
        WriteLn('Il y a quelque chose de pourri dans ce programme.');
        Continue;
      end;
      Inc(result, RecursiveGetMovesCount(LPos, Pred(ADepth)));
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
  WriteLn('Depth   Result   Time elapsed');
  for i := 1 to ADepth do
  begin
    t := GetTickCount64;
    n := RecursiveGetMovesCount(p, i);
    t := GetTickCount64 - t;
    WriteLn(i:5, n:9, FormatDateTime('   hh:nn:ss:zzz', t / (1000 * SECSPERDAY)));
  end;
end;

end.
