
uses
  Board, Tables;

begin
  { Même chose que dans l'exemple précédent, mais avec un damier précalculé. }
  WriteLn(BoardToFmtStr(CPath[A1, H8]));
end.

{
  +   abcdefgh   +

  8   00000000   8
  7   00000010   7
  6   00000100   6
  5   00001000   5
  4   00010000   4
  3   00100000   3
  2   01000000   2
  1   00000000   1

  +   abcdefgh   +
}
