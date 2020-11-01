
uses
  Board, Chess;

{$ASSERTIONS ON}

const
  CPos = 'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3';
  
var
  LPos: TPosition;

begin
  { Initialisation d'une position à partir d'une chaîne FEN. }
  LPos := EncodePosition(CPos);
  { Affichage des pions blancs. }
  WriteLn(BoardToFormattedStr(LPos.Pieces[FALSE] and LPos.Pawns));
  { Production d'une chaîne FEN, qu'on compare avec la chaîne initiale. }
  Assert(DecodePosition(LPos) = CPos);
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
