
uses
  TypInfo, Board, Tables, Display;

var
  p: TPieceType;
  
begin
  for p in TPieceType do
  begin
    WriteLn(GetEnumName(TypeInfo(TPieceType), Ord(p)));
    DisplayBoard(CTargets[p, E2]);
  end;
end.

{
  ptWhitePawn
  +   abcdefgh   +

  8   00000000   8
  7   00000000   7
  6   00000000   6
  5   00000000   5
  4   00000000   4
  3   00010100   3
  2   00000000   2
  1   00000000   1

  +   abcdefgh   +
  ptBlackPawn
  +   abcdefgh   +

  8   00000000   8
  7   00000000   7
  6   00000000   6
  5   00000000   5
  4   00000000   4
  3   00000000   3
  2   00000000   2
  1   00010100   1

  +   abcdefgh   +
  ptRook
  +   abcdefgh   +

  8   00001000   8
  7   00001000   7
  6   00001000   6
  5   00001000   5
  4   00001000   4
  3   00001000   3
  2   11110111   2
  1   00001000   1

  +   abcdefgh   +
  ptKnight
  +   abcdefgh   +

  8   00000000   8
  7   00000000   7
  6   00000000   6
  5   00000000   5
  4   00010100   4
  3   00100010   3
  2   00000000   2
  1   00100010   1

  +   abcdefgh   +
  ptBishop
  +   abcdefgh   +

  8   00000000   8
  7   00000000   7
  6   10000000   6
  5   01000001   5
  4   00100010   4
  3   00010100   3
  2   00000000   2
  1   00010100   1

  +   abcdefgh   +
  ptQueen
  +   abcdefgh   +

  8   00001000   8
  7   00001000   7
  6   10001000   6
  5   01001001   5
  4   00101010   4
  3   00011100   3
  2   11110111   2
  1   00011100   1

  +   abcdefgh   +
  ptKing
  +   abcdefgh   +

  8   00000000   8
  7   00000000   7
  6   00000000   6
  5   00000000   5
  4   00000000   4
  3   00011100   3
  2   00010100   2
  1   00011100   1

  +   abcdefgh   +
}
