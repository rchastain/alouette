
{ http://www.fzibi.com/cchess/bitboards.htm }

uses
  Board;

const
  { Toutes les pièces blanches (position de départ conventionnelle). }
  CWhitePieces = %0000000000000000000000000000000000000000000000001111111111111111;
  { Tous les cavaliers. }
  CKnights = %0100001000000000000000000000000000000000000000000000000001000010;
  
var
  LBoard: TBoard;
  
begin
  { Les cavaliers blancs. }
  LBoard := CWhitePieces and CKnights;
  WriteLn(BoardToFmtStr(LBoard));
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
