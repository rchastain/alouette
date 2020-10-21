
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

procedure AppendMove(const AFrom, ATo: integer; const ACondition: boolean = TRUE);
begin
  if ACondition then
  begin
    Inc(ACount);
    if ACount <= Length(AList) then
      AList[Pred(ACount)] := EncodeMove(AFrom, ATo, ptKing, mtCastling)
    else
      Log.Append('** Cannot append move');
  end;
end;

var
  LPieces,             { Toutes les pièces. }
  LThreats: TBoard;    { Cases menacées par l'adversaire. }
  LRow,
  LKingStartCol,          { Colonne de départ du roi. }
  LRookStartCol: integer; { Colonne de départ de la tour. }
  LPos: TPosition;
  
procedure Search(const AKingTarget, ARookTarget: integer); { Colonnes d'arrivée. }
var
  LKingStartIdx, LRookStartIdx, LKingTargetIdx, LRookTargetIdx: integer;
  LPath: TBoard;  { Chemin à parcourir, y compris la case d'arrivée. }
  LRooks: TBoard; { Pièces autorisées sur le parcours. }
  LKing: TBoard;
begin
  LKingStartIdx := ToIndex(LKingStartCol, LRow);
  LKingTargetIdx := ToIndex(AKingTarget, LRow);
  LRookStartIdx := ToIndex(LRookStartCol, LRow);
  LRookTargetIdx := ToIndex(ARookTarget, LRow);
  Log.Append(Format('** Generate castling king %s rook %s', [
    MoveToStr(LKingStartIdx, LKingTargetIdx),
    MoveToStr(LRookStartIdx, LRookTargetIdx)
  ]));
  
  if IsOn(APos.Pieces[APos.SideToMove] and APos.Rooks, CIndexToSquare[LRookStartIdx]) then
    Log.Append('** Rook on square (cond. 1/3)')
  else
  begin
    Log.Append('** Rook not found');
    Exit;
  end;
  
  LPath :=
    CPath[LKingStartIdx, LKingTargetIdx] or CIndexToSquare[LKingTargetIdx] or
    CPath[LRookStartIdx, LRookTargetIdx] or CIndexToSquare[LRookTargetIdx];
  LRooks := APos.Rooks and APos.Pieces[APos.SideToMove];
  LKing := APos.Kings and APos.Pieces[APos.SideToMove];
  if ((LPath and LPieces) = (LPath and (LRooks or LKing)))
  and (CountSquaresOn(LPath and LRooks) <= 1)
  and (CountSquaresOn(LPath and LKing) <= 1)
  then
    Log.Append('** No piece on the path (cond. 2/3)')
  else
    Exit;
  
  if (LThreats and ((CIndexToSquare[LKingStartIdx] or CPath[LKingStartIdx, LKingTargetIdx] or CIndexToSquare[LKingTargetIdx])) = 0) then
    Log.Append('** No attacked square (cond. 3/3)')
  else
    Exit;
  AppendMove(LKingStartIdx, LRookStartIdx);
end;

begin
  if APos.SideToMove then LRow := CRow8 else LRow := CRow1;
  LPieces := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  LPos := APos;
  LPos.SideToMove := not LPos.SideToMove;
  LThreats := GenMoves(LPos) or GenPotentialPawnMoves(LPos);
  LKingStartCol := SquareToCol(APos.KingSquare[APos.SideToMove]);
  
  LRookStartCol := APos.Roque[APos.SideToMove].KingRookCol;
  if (LRookStartCol >= 0) and (LRookStartCol <= 7) then
    Search(CColG, CColF);
  
  LRookStartCol := APos.Roque[APos.SideToMove].QueenRookCol;
  if (LRookStartCol >= 0) and (LRookStartCol <= 7) then
    Search(CColC, CColD);
end;

end.
