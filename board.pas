
{**
  @abstract(Damier binaire.)
  Représentation d'un damier par un nombre entier de 64 chiffres binaires.
}

unit Board;

interface

type
  {** Le damier est représenté par un nombre entier de 64 chiffres binaires. }
  TBoard = Int64;
  {** Type de pièce, incluant la valeur néant. }
  TWidePieceType = (ptNil, ptWhitePawn, ptBlackPawn, ptRook, ptKnight, ptBishop, ptQueen, ptKing);
  {** Type de pièce. }
  TPieceType = ptWhitePawn..ptKing;
  {** Type de coup. }
  TMoveType = (mtCommon, mtCapture, mtCastling, mtEnPassant, mtPromotion);
  
{** Case pour un nombre donné de 0 à 63. La fonction renvoie un damier avec une seule case allumée. }
function ToBoard(const AIdx: integer): TBoard; overload;
{** Case pour deux nombres donnés de 0 à 7. }
function ToBoard(const ACol, ARow: integer): TBoard; overload;
{** Index pour deux nombres donnés de 0 à 7. }
function ToIndex(const ACol, ARow: integer): integer;
{** Nom d'une case à partir de ses coordonnées. }
function SquareToStr(const ACol, ARow: integer; const AUpCase: boolean = FALSE): string; overload;
{** Nom d'une case à partir de son index. }
function SquareToStr(const AIdx: integer; const AUpCase: boolean = FALSE): string; overload;
{** Nom d'un coup à partir de l'index des cases. }
function MoveToStr(const AFrom, ATo: integer): string; overload;
{** Nom d'un coup à partir de sa représentation par un nombre entier. }
function MoveToStr(const AMove: integer): string; overload;
{** Conversion d'un coup en nombre entier. }
function EncodeMove(const AFrom, ATo: integer; const APieceType: TPieceType; const AMoveType: TMoveType = mtCommon): integer;
{** Conversion d'un nombre entier en cases de départ et d'arrivée. }
procedure DecodeMove(const AMove: integer; out AFrom, ATo: integer); overload;
{** Décodage d'un nombre entier représentant un coup. }
procedure DecodeMove(const AMove: integer; out AFrom, ATo: integer; out APieceType: TPieceType; out AMoveType: TMoveType); overload;
{** Index de la case d'arrivée pour un coup représenté par un nombre entier. }
function TargetIndex(const AMove: integer): integer;
{** Type de pièce pour un coup représenté par un nombre entier. }
function PieceType(const AMove: integer): TPieceType;
{** Type de coup pour un coup représenté par un nombre entier. }
function MoveType(const AMove: integer): TMoveType;
{** Index de la case de départ pour un coup représenté par un nombre entier. }
function StartIndex(const AMove: integer): integer;
{** Convertit une chaîne de la forme "a1" en un nombre de 0 à 63. }
function DecodeSquareName(const AName: string): integer;
{** Pour savoir si une case est allumée dans un damier. }
function IsOn(const ABrd, ASqr: TBoard): boolean;
{** Allume une case dans un damier. }
procedure SwitchOn(var ABrd: TBoard; const ASqr: TBoard);
{** Éteint une case. }
procedure SwitchOff(var ABrd: TBoard; const ASqr: TBoard);
{** Éteint une case et en allume une autre, dans deux damiers à la fois. }
procedure MovePiece(var AType, AColor: TBoard; const AFrom, ATo: TBoard);
{** Chaîne de chiffres binaires. }
function BoardToStr(const ABrd: TBoard): string;
{** Chaîne de chiffres binaires en forme de damier. }
function BoardToFormattedStr(const ABrd: TBoard): string;
{** Pour savoir si la nature d'une pièce lui permet tel déplacement. }
function IsMovePossible(const APiece: TPieceType; const AX1, AY1, AX2, AY2: integer): boolean;
{** Toutes les cases que la pièce, selon son type, peut atteindre. }
function GetTargets(const APiece: TPieceType; const AIdx: integer): TBoard;
{** Les cases à traverser pour aller d'un endroit à un autre. }
function GetPath(const AFrom, ATo: integer): TBoard;

const
  {** Numérotation des cases de 0 à 63. }
  A1 = 00; B1 = 01; C1 = 02; D1 = 03; E1 = 04; F1 = 05; G1 = 06; H1 = 07;
  A2 = 08; B2 = 09; C2 = 10; D2 = 11; E2 = 12; F2 = 13; G2 = 14; H2 = 15;
  A3 = 16; B3 = 17; C3 = 18; D3 = 19; E3 = 20; F3 = 21; G3 = 22; H3 = 23;
  A4 = 24; B4 = 25; C4 = 26; D4 = 27; E4 = 28; F4 = 29; G4 = 30; H4 = 31;
  A5 = 32; B5 = 33; C5 = 34; D5 = 35; E5 = 36; F5 = 37; G5 = 38; H5 = 39;
  A6 = 40; B6 = 41; C6 = 42; D6 = 43; E6 = 44; F6 = 45; G6 = 46; H6 = 47;
  A7 = 48; B7 = 49; C7 = 50; D7 = 51; E7 = 52; F7 = 53; G7 = 54; H7 = 55;
  A8 = 56; B8 = 57; C8 = 58; D8 = 59; E8 = 60; F8 = 61; G8 = 62; H8 = 63;
  
  CSqrToStr: array [0..63] of string[2] = (
    'a1', 'b1', 'c1', 'd1', 'e1', 'f1', 'g1', 'h1',
    'a2', 'b2', 'c2', 'd2', 'e2', 'f2', 'g2', 'h2',
    'a3', 'b3', 'c3', 'd3', 'e3', 'f3', 'g3', 'h3',
    'a4', 'b4', 'c4', 'd4', 'e4', 'f4', 'g4', 'h4',
    'a5', 'b5', 'c5', 'd5', 'e5', 'f5', 'g5', 'h5',
    'a6', 'b6', 'c6', 'd6', 'e6', 'f6', 'g6', 'h6',
    'a7', 'b7', 'c7', 'd7', 'e7', 'f7', 'g7', 'h7',
    'a8', 'b8', 'c8', 'd8', 'e8', 'f8', 'g8', 'h8'
  );

implementation

uses
  SysUtils;
  
function ToBoard(const AIdx: integer): TBoard;
begin
  Assert((AIdx >= 0) and (AIdx <= 63));
  result := TBoard(1) shl AIdx;
end;

function ToBoard(const ACol, ARow: integer): TBoard;
begin
  Assert((ACol >= 0) and (ACol <= 7) and (ARow >= 0) and ( ARow <= 7));
  result := ToBoard(8 * ARow + ACol);
end;

function ToIndex(const ACol, ARow: integer): integer;
begin
  result := 8 * ARow + ACol;
end;

function SquareToStr(const ACol, ARow: integer; const AUpCase: boolean): string;
begin
  Assert((ACol >= 0) and (ACol <= 7) and (ARow >= 0) and ( ARow <= 7));
  result := Concat(
    Chr(ACol + Ord('a') + (Ord('A') - Ord('a')) * Ord(AUpCase)),
    Chr(ARow + Ord('1'))
  );
end;

function SquareToStr(const AIdx: integer; const AUpCase: boolean): string;
begin
  result := SquareToStr(AIdx mod 8, AIdx div 8, AUpCase);
end;

function MoveToStr(const AFrom, ATo: integer): string;
begin
  result := Concat(SquareToStr(AFrom), SquareToStr(ATo));
end;

function EncodeMove(const AFrom, ATo: integer; const APieceType: TPieceType; const AMoveType: TMoveType): integer;
begin
  result :=
    Ord(APieceType) shl 24
  + Ord(AMoveType)  shl 16
  + AFrom           shl  8
  + ATo;
end;

procedure DecodeMove(const AMove: integer; out AFrom, ATo: integer);
begin
  AFrom := (AMove and $0000FF00) shr 8;
  ATo := (AMove and $000000FF);
end;

procedure DecodeMove(const AMove: integer; out AFrom, ATo: integer; out APieceType: TPieceType; out AMoveType: TMoveType); overload;
begin
  DecodeMove(AMove, AFrom, ATo);
  APieceType := TPieceType((AMove and $FF000000) shr 24);
  AMoveType := TMoveType ((AMove and $00FF0000) shr 16);
end;

function TargetIndex(const AMove: integer): integer;
begin
  result := (AMove and $000000FF) shr 0;
end;

function StartIndex(const AMove: integer): integer;
begin
  result := (AMove and $0000FF00) shr 8;
end;

function PieceType(const AMove: integer): TPieceType;
begin
  result := TPieceType((AMove and $FF000000) shr 24);
end;

function MoveType(const AMove: integer): TMoveType;
begin
  result := TMoveType ((AMove and $00FF0000) shr 16);
end;

function MoveToStr(const AMove: integer): string;
var
  LStartIndex, LArriv: integer;
begin
  DecodeMove(AMove, LStartIndex, LArriv);
  result := Concat(SquareToStr(LStartIndex), SquareToStr(LArriv));
end;

function DecodeSquareName(const AName: string): integer;
begin
  Assert((Length(AName) = 2) and (AName[1] in ['a'..'h']) and (AName[2] in ['1'..'8']));
  result := 8 * (Ord(AName[2]) - Ord('1')) + (Ord(AName[1]) - Ord('a'));
end;

function IsOn(const ABrd, ASqr: TBoard): boolean;
begin
  Assert(ASqr <> 0);
  result := (ABrd and ASqr) = ASqr;
end;

procedure SwitchOn(var ABrd: TBoard; const ASqr: TBoard);
begin
  ABrd := ABrd or ASqr;
end;

procedure SwitchOff(var ABrd: TBoard; const ASqr: TBoard);
begin
  ABrd := ABrd and not ASqr;
end;

procedure MovePiece(var AType, AColor: TBoard; const AFrom, ATo: TBoard);
begin
  AType := AType and not AFrom or ATo;
  AColor := AColor and not AFrom or ATo;
end;

function BoardToStr(const ABrd: TBoard): string;
const
  CChar: array[boolean] of char = ('0', '1');
var
  i: integer;
begin
  SetLength(result, 64);
  for i := 63 downto 0 do
    result[64 - i] := CChar[IsOn(ABrd, ToBoard(i))];
end;

function BoardToFormattedStr(const ABrd: TBoard): string;
var
  x, y: integer;
  LBrd: string;
begin
  LBrd := BoardToStr(ABrd);
  result := '+   abcdefgh   +' + LineEnding + LineEnding;
  for y := 7 downto 0 do
  begin
    result := Concat(result, IntToStr(Succ(y)), '   ');
    for x := 0 to 7 do
      result := Concat(result, LBrd[64 - 8 * y - x]);
    result := Concat(result, '   ', IntToStr(Succ(y)), LineEnding);
  end;
  result := Concat(result, LineEnding, '+   abcdefgh   +');
end;

function IsMovePossible(const APiece: TPieceType; const AX1, AY1, AX2, AY2: integer): boolean;
var
  dx, dy, ax, ay: integer;
begin
  dx := AX2 - AX1;
  dy := AY2 - AY1;
  ax := Abs(dx);
  ay := Abs(dy);
  case APiece of
    ptWhitePawn: result := (dy = 1) and (ax = 1);
    ptBlackPawn: result := (dy = -1) and (ax = 1);
    ptRook:      result := (dx = 0) xor (dy = 0);
    ptKnight:    result := ax * ay = 2;
    ptBishop:    result := (dx <> 0) and (ax = ay);
    ptQueen:     result := IsMovePossible(ptRook, AX1, AY1, AX2, AY2) or IsMovePossible(ptBishop, AX1, AY1, AX2, AY2);
    ptKing:      result := (ax + ay <= 2) and ((ax = 1) or (ay = 1));
  end;
end;

function GetTargets(const APiece: TPieceType; const AIdx: integer): TBoard;
var
  x1, y1, x2, y2: integer;
begin
  x1 := AIdx mod 8;
  y1 := AIdx div 8;
  result := 0;
  for y2 := 7 downto 0 do
    for x2 := 0 to 7 do
      if IsMovePossible(APiece, x1, y1, x2, y2) then
        SwitchOn(result, ToBoard(x2, y2));
end;

function GetPath(const AFrom, ATo: integer): TBoard;
var
  x1, y1, x2, y2, dx, dy: integer;
begin
  result := 0;
  x1 := AFrom mod 8;
  y1 := AFrom div 8;
  x2 := ATo mod 8;
  y2 := ATo div 8;
  dx := x2 - x1;
  dy := y2 - y1;
  if ((dx <> 0) or (dy <> 0))
  and (((dx = 0) or (dy = 0)) or (Abs(dx) = Abs(dy))) then
  begin
    if dx <> 0 then dx := dx div Abs(dx);
    if dy <> 0 then dy := dy div Abs(dy);
    repeat
      Inc(x1, dx);
      Inc(y1, dy);
      if (x1 <> x2)
      or (y1 <> y2) then
        result := result or ToBoard(x1, y1);
    until (x1 = x2) and (y1 = y2);
  end;
end;

end.
