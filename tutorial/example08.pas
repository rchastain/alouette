
uses
  SysUtils, Board, Tables, Chess, Moves, Castling;

const
  CPos: array[0..1] of string = (
    'bbnnqrkr/pppppppp/8/8/8/8/PPPPPPPP/BBNNQRKR w HFhf -',
    'bnrqknrb/pp1ppppp/2p5/8/8/4N3/PPPPPPPP/BNRQK1RB w GCgc -'
  );

const
  CVariant = TRUE;
  
var
  p: TPosition;
  mm: array[0..99] of integer;
  n, i: integer;

begin
  p := EncodePosition(CPos[1], CVariant);
  Assert(DecodePosition(p, CVariant) = CPos[1]);
  GenMoves(p, mm, n);
  GenCastling(p, mm, n);
  for i := 0 to Pred(n) do
    WriteLn(MoveToStr(mm[i]));
end.
