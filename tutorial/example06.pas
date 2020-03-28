
uses
  Board, Chess, Display;

{$ASSERTIONS ON}

const
  CPos = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3';
  
var
  p: TPosition;

begin
  p := EncodePosition(CPos);
  DisplayBoard(p.Pieces[FALSE] and p.Pawns);
  Assert(DecodePosition(p) = CPos);
end.

{
  +   abcdefgh   +

  8   00000000   8
  7   00000000   7
  6   00000000   6
  5   00000000   5
  4   00001000   4
  3   00000000   3
  2   11110111   2
  1   00000000   1

  +   abcdefgh   +
}
