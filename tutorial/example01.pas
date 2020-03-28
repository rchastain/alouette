
{ http://www.fzibi.com/cchess/bitboards.htm }

uses
  Board, Display;

const
  CWhitePieces = %0000000000000000000000000000000000000000000000001111111111111111;
  CKnights = %0100001000000000000000000000000000000000000000000000000001000010;
  
var
  b: TBoard;
  
begin
  b := CWhitePieces and CKnights;
  DisplayBoard(b);
end.

{
  +   abcdefgh   +

  8   00000000   8
  7   00000000   7
  6   00000000   6
  5   00000000   5
  4   00000000   4
  3   00000000   3
  2   00000000   2
  1   01000010   1

  +   abcdefgh   +
}
