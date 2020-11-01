
uses
  SysUtils, Board, Tables, Chess, Moves, Castling;

const
  CPos: array[0..1] of string = (
    'bbnnqrkr/pppppppp/8/8/8/8/PPPPPPPP/BBNNQRKR w HFhf -',
    'bnrqknrb/pp1ppppp/2p5/8/8/4N3/PPPPPPPP/BNRQK1RB w GCgc -'
  );

{ Génération des coups pour une position donnée des échecs aléatoires. }

const
  CVariant = TRUE;
  
var
  LPos: TPosition;
  LMoves: array[0..99] of integer;
  LCount, i: integer;

begin
  LPos := EncodePosition(CPos[1], CVariant);
  Assert(DecodePosition(LPos, CVariant) = CPos[1]);
  GenMoves(LPos, LMoves, LCount);
  GenCastling(LPos, LMoves, LCount);
  for i := 0 to Pred(LCount) do
    WriteLn(MoveToStr(LMoves[i]));
end.
