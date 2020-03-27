
{**
  @abstract(Roque.)
  Génération du roque.
}

unit Castling;

interface

uses
  Chess, Log;

procedure GenCastling(const APos: TPosition; var AList: array of integer; var ACount: integer);

implementation

uses
  SysUtils, Board, Tables, Moves;

procedure GenCastling(const APos: TPosition; var AList: array of integer; var ACount: integer);
procedure AppendMove(const i, j: integer; const ACondition: boolean = TRUE);
begin
  if ACondition then
  begin
    Inc(ACount);
    Assert(ACount <= Length(AList));
    AList[Pred(ACount)] := EncodeMove(i, j, ptKing, mtCastling);
  end;
end;
var
  LAllPieces,                 { Toutes les pièces. }
  LThreatenedSquares: TBoard; { Cases menacées par l'adversaire. }
  LRow,
  LKingStart,                 { Colonne de départ du roi. }
  LRookStart: integer;        { Colonne de départ de la tour. }
  LPos: TPosition;
procedure Search(const AKingTarget, ARookTarget: integer); { Colonnes d'arrivée. }
var
  i, j, k, l: integer;
  b, c, d: boolean;
  LPath: TBoard;  { Chemin à parcourir, y compris la case d'arrivée. }
  LRooks: TBoard; { Pièces autorisées sur le parcours. }
begin
  i := ToIndex(LKingStart, LRow);
  k := ToIndex(AKingTarget, LRow);
  j := ToIndex(LRookStart, LRow);
  l := ToIndex(ARookTarget, LRow);
  Log.Append(Format('Vérifications pour roi %s tour %s...', [MoveToStr(i, k), MoveToStr(j, l)]));
  if IsOn(APos.Pieces[APos.SideToMove] and APos.Rooks, CIndexToSquare[j]) then
    Log.Append('Position tour vérifiée (condition 1/3).')
  else
    Exit;
  LPath := CPath[i, k] or CIndexToSquare[k];
  LRooks := APos.Rooks and APos.Pieces[APos.SideToMove];
  b := (LPath and LAllPieces) = (LPath and LRooks);
  c := (CountSquaresOn(LPath and LRooks) <= 1);
  LPath := CPath[j, l] or CIndexToSquare[l];
  LRooks := APos.Kings and APos.Pieces[APos.SideToMove];
  d := (LPath and LAllPieces) = (LPath and LRooks);
  if b and c and d then
    Log.Append('Liberté de passage vérifiée (condition 2/3).')
  else
    Exit;
  if (LThreatenedSquares and ((CIndexToSquare[i] or CPath[i, k] or CIndexToSquare[k])) = 0) then
    Log.Append('Absence d''empêchement vérifiée (condition 3/3). Roque accepté.')
  else
    Exit;
  AppendMove(i, j);
end;
begin
  with APos do
  begin
    if SideToMove then
      LRow := CRow8
    else
      LRow := CRow1;
    LAllPieces := Pieces[FALSE] or Pieces[TRUE];
  end;
  LPos := APos;
  LPos.SideToMove := not LPos.SideToMove;
  LThreatenedSquares := GenMoves(LPos) or GenPotentialPawnMoves(LPos);
  LKingStart := SquareToCol(APos.KingSquare[APos.SideToMove]);
  LRookStart := APos.Roque[APos.SideToMove].KingRookCol;
  if (LRookStart >= 0) and (LRookStart <= 7) then
    Search(CColG, CColF);
  LRookStart := APos.Roque[APos.SideToMove].QueenRookCol;
  if (LRookStart >= 0) and (LRookStart <= 7) then
    Search(CColC, CColD);
end;

end.
