
{**
  @abstract(Roque.)
  Génération du roque.
}

unit Castling;

interface

uses
  Chess, Board, Log;

procedure GenCastling(const APos: TPosition; var AList: array of TMove; var ACount: integer);

implementation

uses
  SysUtils, Tables, Moves;

procedure GenCastling(const APos: TPosition; var AList: array of TMove; var ACount: integer);

  procedure SaveMove(const AFr, ATo: integer);
  begin
    Inc(ACount);
    if ACount <= Length(AList) then
      AList[Pred(ACount)] := EncodeMove(AFr, ATo, ptKing, [mtCastling])
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
  LKFrCol,
{** Colonne de départ de la tour. }
  LRFrCol: integer;
  LPos: TPosition;
  
procedure Search(const AKToCol, ARToCol: integer); { Colonnes d'arrivée. }
var
  LKFrIdx, LRFrIdx, LKToIdx, LRToIdx: integer;
{ Chemin à parcourir, y compris la case d'arrivée. }
  LKingPath, LRookPath, LPath: TBoard;
{ Pièces autorisées sur le parcours. }
  LRooks, LKing: TBoard;
begin
  LKFrIdx := ToIndex(LKFrCol, LRow);
  LKToIdx := ToIndex(AKToCol, LRow);
  LRFrIdx := ToIndex(LRFrCol, LRow);
  LRToIdx := ToIndex(ARToCol, LRow);
{$IFDEF DEBUG_CASTLING}
  Log.Append(Format('** Generate castling king %s rook %s', [MoveToStr(LKFrIdx, LKToIdx), MoveToStr(LRFrIdx, LRToIdx)]));
{$ENDIF}
{ Vérification de la première condition : il y a bien une tour à l'endroit prévu. }
  with APos do if IsOn(Pieces[Side] and Rooks, CIdxToSqr[LRFrIdx]) then
{$IFDEF DEBUG_CASTLING}
    Log.Append('** Rook on square (cond. 1/3)')
{$ENDIF}
  else
    Exit;

{ Deuxième condition : aucune pièce n'est sur le passage du roi ni sur celui de la tour. }
  LKingPath := CPath[LKFrIdx, LKToIdx] or CIdxToSqr[LKToIdx];
  LRookPath := CPath[LRFrIdx, LRToIdx] or CIdxToSqr[LRToIdx];
{$IFDEF DEBUG_CASTLING}
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
  and (PopCnt(QWord(LPath and LRooks)) <= 1)
  and (PopCnt(QWord(LPath and LKing)) <= 1)
  then
{$IFDEF DEBUG_CASTLING}
    Log.Append('** Path free (cond. 2/3)')
{$ENDIF}
  else
    Exit;

{ Dernière condition : aucune des cases sur lesquelles le roi se trouve ou se trouvera n'est menacée. }
  LKingPath := CIdxToSqr[LKFrIdx] or CPath[LKFrIdx, LKToIdx] or CIdxToSqr[LKToIdx];
{$IFDEF DEBUG_CASTLING}
  Log.Append(Concat('** Threats:', LineEnding, BoardToFormattedStr(LThreats)));
{$ENDIF}
  if (LThreats and LKingPath) = 0 then
{$IFDEF DEBUG_CASTLING}
    Log.Append('** No attacked square (cond. 3/3)')
{$ENDIF}
  else
    Exit;

{ Enregistrement du coup. Le coup est noté comme la prise de la tour par le roi. }
  SaveMove(LKFrIdx, LRFrIdx);
end;

begin
  LRow := CCastlingRow[APos.Side];
  LPieces := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  LPos := APos;
  LPos.Side := not LPos.Side;
  LThreats := GenMoves(LPos) or GenPotentialPawnMoves(LPos);
  LKFrCol := SquareToCol(APos.KingSquare[APos.Side]);
  LRFrCol := APos.Castling[APos.Side].HRookCol;
  if (LRFrCol >= 0) and (LRFrCol <= 7) then
    Search(CColG, CColF);
  LRFrCol := APos.Castling[APos.Side].ARookCol;
  if (LRFrCol >= 0) and (LRFrCol <= 7) then
    Search(CColC, CColD);
end;

end.
