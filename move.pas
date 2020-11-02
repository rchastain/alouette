
{**
  @abstract(Déplacement des pièces.)
  Complément de l'unité Chess.
}

unit Move;

interface

uses
  Board, Chess;

function PieceTypeIdx(const APos: TPosition; const AIdx: integer): TWidePieceType;
{** Met à jour la position en fonction d'un coup présumé légal. Renvoie FALSE si le coup est impossible ou illégal. }
function DoMove(var APos: TPosition; const AMove: string): boolean;
function IsPromotion(const APos: TPosition; const AMove: string): boolean;
function IsCastling(const APos: TPosition; const AMove: integer): boolean;
procedure RenameCastling(var AMove: integer);

implementation

uses
  SysUtils, Tables, Log;

function PieceTypeIdx(const APos: TPosition; const AIdx: integer): TWidePieceType;
begin
  if      IsOnIdx(APos.Pawns,   AIdx) then if APos.Side then result := ptBlackPawn else result := ptWhitePawn
  else if IsOnIdx(APos.Rooks,   AIdx) then result := ptRook
  else if IsOnIdx(APos.Knights, AIdx) then result := ptKnight
  else if IsOnIdx(APos.Bishops, AIdx) then result := ptBishop
  else if IsOnIdx(APos.Queens,  AIdx) then result := ptQueen
  else if IsOnIdx(APos.Kings,   AIdx) then result := ptKing
  else
    result := ptNil;
end;

function DoMove(var APos: TPosition; const AMove: string): boolean;
var
  LFrom, LTo, LFromCol, LToCol, LFromRow, LToRow, LEnPassantCapture: integer;
  LType: TPieceType;
  LPreserve: boolean;
begin
  result := TRUE;
  
  { Index des cases de départ et d'arrivée. }
  LFrom := DecodeSquareName(Copy(AMove, 1, 2));
  LTo := DecodeSquareName(Copy(AMove, 3, 2));
  
  Assert(IsOnIdx(APos.Pieces[APos.Side], LFrom), 'Cannot evaluate piece color');
  
  if      IsOnIdx(APos.Pawns,   LFrom) then if APos.Side then LType := ptBlackPawn else LType := ptWhitePawn
  else if IsOnIdx(APos.Rooks,   LFrom) then LType := ptRook
  else if IsOnIdx(APos.Knights, LFrom) then LType := ptKnight
  else if IsOnIdx(APos.Bishops, LFrom) then LType := ptBishop
  else if IsOnIdx(APos.Queens,  LFrom) then LType := ptQueen
  else if IsOnIdx(APos.Kings,   LFrom) then LType := ptKing
  else
    Assert(FALSE, 'Cannot evaluate piece type');

  LFromCol := LFrom mod 8;
  LToCol   := LTo   mod 8;
  LFromRow := LFrom div 8;
  LToRow   := LTo   div 8;
  LPreserve := FALSE;
  
  if LType = ptKing then
  begin
    if IsOnIdx(APos.Rooks and APos.Pieces[APos.Side], LTo) then
    begin
      if LToCol = APos.Roque[APos.Side].KingRookCol then
      begin
        Log.Append(Format('** Castling on H side: %s', [AMove]));
        MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], LTo, CATCR[APos.Side]);
        LTo := ToIndex(CColG, LToRow);
        LPreserve := LFromCol = CATCR[APos.Side] mod 8;
      end else
        if LToCol = APos.Roque[APos.Side].QueenRookCol then
        begin
          Log.Append(Format('** Castling on A side: %s', [AMove]));
          MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], LTo, CATCD[APos.Side]);
          LTo := ToIndex(CColC, LToRow);
          LPreserve := LFromCol = CATCD[APos.Side] mod 8;
        end else
        begin
          Log.Append(Format('** Impossible move: %s', [AMove]));
          Exit(FALSE);
        end;
    end else
      if Abs(LToCol - LFromCol) = 2 then
      begin
        if LToCol = CColG then
        begin
          Log.Append(Format('** Castling on king side: %s', [AMove]));
          MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], CDTCR[APos.Side], CATCR[APos.Side]);
        end else
          if LToCol = CColC then
          begin
            Log.Append(Format('** Castling on queen side: %s', [AMove]));
            MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], CDTCD[APos.Side], CATCD[APos.Side]);
          end else
          begin
            Log.Append(Format('** Impossible move: %s', [AMove]));
            Exit(FALSE);
          end;
      end;
    
    APos.Roque[APos.Side].KingRookCol := CNil;
    APos.Roque[APos.Side].QueenRookCol := CNil;
    APos.KingSquare[APos.Side] := CIdxToSqr[LTo];
  end;
  
  { Si la pièce déplacée est une tour... }
  if LType = ptRook then
    with APos.Roque[APos.Side] do
      if LFromCol = KingRookCol then
        KingRookCol := CNil
      else
      if LFromCol = QueenRookCol then
        QueenRookCol := CNil;
  
  { S'il y a une pièce sur la case d'arrivée... }
  if IsOnIdx(APos.Pieces[not APos.Side], LTo) then
  begin
    if IsOnIdx(APos.Rooks, LTo)
    and (LToRow = CCastlingRow[not APos.Side]) then
      with APos.Roque[not APos.Side] do
        if (LToCol = KingRookCol) then
          KingRookCol := CNil
        else
        if LToCol = KingRookCol then
          KingRookCol := CNil;
        
    with APos do
    begin
      SwitchOffIdx(Pawns,   LTo);
      SwitchOffIdx(Rooks,   LTo);
      SwitchOffIdx(Knights, LTo);
      SwitchOffIdx(Bishops, LTo);
      SwitchOffIdx(Queens,  LTo);
      SwitchOffIdx(Kings,   LTo);
    end;
    SwitchOffIdx(APos.Pieces[not APos.Side], LTo);
  end;
  
  { Si la pièce déplacée est un pion... }
  if (LType = ptWhitePawn) or (LType = ptBlackPawn) then
  begin
    { Promotion. }
    if (Length(AMove) = 4) and IsPromotion(APos, AMove) then
    begin
      SwitchOffIdx(APos.Pawns, LFrom);
      SwitchOnIdx(APos.Queens, LFrom);
      LType := ptQueen;
    end else
      if (Length(AMove) = 5) then
        case AMove[5] of
          'n':
            begin
              SwitchOffIdx(APos.Pawns, LFrom);
              SwitchOnIdx(APos.Knights, LFrom);
              LType := ptKnight;
            end;
          'b':
            begin
              SwitchOffIdx(APos.Pawns, LFrom);
              SwitchOnIdx(APos.Bishops, LFrom);
              LType := ptBishop;
            end;
          'r':
            begin
              SwitchOffIdx(APos.Pawns, LFrom);
              SwitchOnIdx(APos.Rooks, LFrom);
              LType := ptRook;
            end;
          'q':
            begin
              SwitchOffIdx(APos.Pawns, LFrom);
              SwitchOnIdx(APos.Queens, LFrom);
              LType := ptQueen;
            end;
          else
            Log.Append(Format('** Unexpected value: %s', [AMove[5]]));    
        end;
    
    { Prise en passant. }
    if LTo = APos.EnPassant then
    begin
      LEnPassantCapture := ToIndex(LToCol, LFromRow);
      SwitchOffIdx(APos.Pawns, LEnPassantCapture);
      SwitchOffIdx(APos.Pieces[not APos.Side], LEnPassantCapture);
    end;
  end;
  
  if ((LType = ptWhitePawn) or (LType = ptBlackPawn))
  and (Abs(LToRow - LFromRow) = 2) then
    APos.EnPassant := ToIndex(LFromCol, LFromRow + (LToRow - LFromRow) div 2)
  else
    APos.EnPassant := CNil;
  
  { Déplacement de la pièce. }
  case LType of
    ptWhitePawn,
    ptBlackPawn: MovePieceIdx(APos.Pawns,   APos.Pieces[APos.Side], LFrom, LTo, LPreserve);
    ptRook:      MovePieceIdx(APos.Rooks,   APos.Pieces[APos.Side], LFrom, LTo, LPreserve);
    ptKnight:    MovePieceIdx(APos.Knights, APos.Pieces[APos.Side], LFrom, LTo, LPreserve);
    ptBishop:    MovePieceIdx(APos.Bishops, APos.Pieces[APos.Side], LFrom, LTo, LPreserve);
    ptQueen:     MovePieceIdx(APos.Queens,  APos.Pieces[APos.Side], LFrom, LTo, LPreserve);
    ptKing:      MovePieceIdx(APos.Kings,   APos.Pieces[APos.Side], LFrom, LTo, LPreserve);
  end;
  { Changement du trait. }
  APos.Side := not APos.Side;
end;

function IsPromotion(const APos: TPosition; const AMove: string): boolean;
var
  LFrom, LTo: integer;
begin
  LFrom := DecodeSquareName(Copy(AMove, 1, 2));
  LTo := DecodeSquareName(Copy(AMove, 3, 2)) div 8;
  result := IsOnIdx(APos.Pawns, LFrom) and (
    not APos.Side and (LTo = CRow8)
    or  APos.Side and (LTo = CRow1)
  );
end;

function IsCastling(const APos: TPosition; const AMove: integer): boolean;
var
  LFrom, LTo: integer;
  LBlanc, LNoir: TBoard;
begin
  LBlanc := APos.Pieces[FALSE];
  LNoir := APos.Pieces[TRUE];
  DecodeMove(AMove, LFrom, LTo);
  result :=
    (IsOnIdx(LBlanc, LFrom) and IsOnIdx(LBlanc, LTo)) or
    (IsOnIdx(LNoir,  LFrom) and IsOnIdx(LNoir,  LTo));
  if result then
    Log.Append(Format('** Castling move: %s', [MoveToStr(AMove)]));
end;

procedure RenameCastling(var AMove: integer);
var
  LFrom, LTo, LFromRow, LToRow, LToCol: integer;
  LAncienNom: string;
begin
  DecodeMove(AMove, LFrom, LTo);
  Assert((LFrom >= 0) and (LFrom <= 63) and (LTo >= 0) and (LTo <= 63));
  LAncienNom := Concat(CSqrToStr[LFrom], CSqrToStr[LTo]);
  LFromRow := LFrom div 8;
  LToRow := LTo div 8;
  Assert((LToRow = LFromRow) and ((LFromRow = CRow1) or (LFromRow = CRow8)));
  if LTo mod 8 > LFrom mod 8 then
    LToCol := CColG
  else
    LToCol := CColC;
  LTo := 8 * LToRow + LToCol;
  AMove := EncodeMove(LFrom, LTo, ptKing);
  Log.Append(Format('** Reformulated %s to %s', [LAncienNom, Concat(CSqrToStr[LFrom], CSqrToStr[LTo])]));
end;

end.
