
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

  procedure SaveMove(const AFrom, ATo: integer);
  begin
    Inc(ACount);
    if ACount <= Length(AList) then
      AList[Pred(ACount)] := EncodeMove(AFrom, ATo, ptKing, mtCastling)
    else
      Log.Append('** Cannot append move');
  end;

var
{** Toutes les pièces. }
  LPieces,
{** Cases menacées par l'adversaire. }
  LThreats: TBoard;
  LRow,
{** Colonne de départ du roi. }
  LKingFromCol,
{** Colonne de départ de la tour. }
  LRookFromCol: integer;
  LPos: TPosition;
  
procedure Search(const AKingToCol, ARookToCol: integer); { Colonnes d'arrivée. }
var
  LKingFromIdx, LRookFromIdx, LKingToIdx, LRookToIdx: integer;
{ Chemin à parcourir, y compris la case d'arrivée. }
  LKingPath, LRookPath, LPath: TBoard;
{ Pièces autorisées sur le parcours. }
  LRooks, LKing: TBoard;
begin
  LKingFromIdx := ToIndex(LKingFromCol, LRow);
  LKingToIdx := ToIndex(AKingToCol, LRow);
  LRookFromIdx := ToIndex(LRookFromCol, LRow);
  LRookToIdx := ToIndex(ARookToCol, LRow);
  Log.Append(Format('** Generate castling king %s rook %s', [MoveToStr(LKingFromIdx, LKingToIdx), MoveToStr(LRookFromIdx, LRookToIdx)]));

{ Vérification de la première condition : il y a bien une tour à l'endroit prévu. }
  with APos do if IsOn(Pieces[Side] and Rooks, CIdxToSqr[LRookFromIdx]) then
    Log.Append('** Rook on square (cond. 1/3)')
  else
    Exit;

{ Deuxième condition : aucune pièce n'est sur le passage du roi ni sur celui de la tour. }
  LKingPath := CPath[LKingFromIdx, LKingToIdx] or CIdxToSqr[LKingToIdx];
  LRookPath := CPath[LRookFromIdx, LRookToIdx] or CIdxToSqr[LRookToIdx];
{$IFDEF DEBUG}
  Log.Append(Concat(
    '** King path:',  LineEnding, BoardToFormattedStr(LKingPath), LineEnding,
    '** Rook path:',  LineEnding, BoardToFormattedStr(LRookPath), LineEnding,
    '** All pieces:', LineEnding, BoardToFormattedStr(LPieces)
  ));
{$ENDIF}
  LPath := LKingPath or LRookPath;
  with APos do
  begin
    LRooks := Rooks and Pieces[Side];
    LKing  := Kings and Pieces[Side];
  end;
  if ((LPath and LPieces) = (LPath and (LRooks or LKing)))
  and (CountSquaresOn(LPath and LRooks) <= 1)
  and (CountSquaresOn(LPath and LKing) <= 1)
  then
    Log.Append('** Path free (cond. 2/3)')
  else
    Exit;

{ Dernière condition : aucune des cases sur lesquelles le roi se trouve ou se trouvera n'est menacée. }
  LKingPath := CIdxToSqr[LKingFromIdx] or CPath[LKingFromIdx, LKingToIdx] or CIdxToSqr[LKingToIdx];
{$IFDEF DEBUG}
  Log.Append(Concat('** Threats:', LineEnding, BoardToFormattedStr(LThreats)));
{$ENDIF}
  if (LThreats and LKingPath) = 0 then
    Log.Append('** No attacked square (cond. 3/3)')
  else
    Exit;

{ Enregistrement du coup. Le coup est noté comme la prise de la tour par le roi. }
  SaveMove(LKingFromIdx, LRookFromIdx);
end;

begin
  if APos.Side then LRow := CRow8 else LRow := CRow1;
  LPieces := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  LPos := APos;
  LPos.Side := not LPos.Side;
  LThreats := GenMoves(LPos) or GenPotentialPawnMoves(LPos);
  LKingFromCol := SquareToCol(APos.KingSquare[APos.Side]);
  
  LRookFromCol := APos.Roque[APos.Side].KingRookCol;
  if (LRookFromCol >= 0) and (LRookFromCol <= 7) then
    Search(CColG, CColF);
  
  LRookFromCol := APos.Roque[APos.Side].QueenRookCol;
  if (LRookFromCol >= 0) and (LRookFromCol <= 7) then
    Search(CColC, CColD);
end;

end.
