
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
function DoMove(var APos: TPosition; const AMove: TMove): boolean;
function DoMove(var APos: TPosition; const AMove: string): boolean;
function IsPromotion(const APos: TPosition; const AMove: string): boolean;
function IsCastling(const APos: TPosition; const AMove: TMove): boolean;
procedure RenameCastling(var AMove: TMove);

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

function DoMove(var APos: TPosition; const AMove: TMove): boolean;
var
  LFr, LTo, LFrCol, LToCol, LFrRow, LToRow, LEnPassantCapture: integer;
  LPieceType: TPieceType;
  LMoveType: TMoveTypeSet;
  LPreserve: boolean;
begin
  result := TRUE;
  DecodeMove(AMove, LFr, LTo, LPieceType, LMoveType);
  Assert(IsOnIdx(APos.Pieces[APos.Side], LFr));
  
  if      IsOnIdx(APos.Pawns,   LFr) then if APos.Side then LPieceType := ptBlackPawn else LPieceType := ptWhitePawn
  else if IsOnIdx(APos.Rooks,   LFr) then LPieceType := ptRook
  else if IsOnIdx(APos.Knights, LFr) then LPieceType := ptKnight
  else if IsOnIdx(APos.Bishops, LFr) then LPieceType := ptBishop
  else if IsOnIdx(APos.Queens,  LFr) then LPieceType := ptQueen
  else if IsOnIdx(APos.Kings,   LFr) then LPieceType := ptKing
  else
    Assert(FALSE);

  LFrCol := LFr mod 8;
  LToCol := LTo mod 8;
  LFrRow := LFr div 8;
  LToRow := LTo div 8;
  LPreserve := FALSE;
  
  if LPieceType = ptKing then
  begin
    if IsOnIdx(APos.Rooks and APos.Pieces[APos.Side], LTo) then
    begin
      if LToCol = APos.Roque[APos.Side].KingRookCol then
      begin
        //Log.Append(Format('** Castling on H side: %s', [AMove]));
        MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], LTo, CATCR[APos.Side]);
        LTo := ToIndex(CColG, LToRow);
        LPreserve := LFrCol = CATCR[APos.Side] mod 8;
      end else
        if LToCol = APos.Roque[APos.Side].QueenRookCol then
        begin
          //Log.Append(Format('** Castling on A side: %s', [AMove]));
          MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], LTo, CATCD[APos.Side]);
          LTo := ToIndex(CColC, LToRow);
          LPreserve := LFrCol = CATCD[APos.Side] mod 8;
        end else
        begin
          Log.Append(Format('** Impossible move: %s', [MoveToStr(AMove)]));
          Exit(FALSE);
        end;
    end else
      if Abs(LToCol - LFrCol) = 2 then
      begin
        if LToCol = CColG then
        begin
          //Log.Append(Format('** Castling on king side: %s', [AMove]));
          MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], CDTCR[APos.Side], CATCR[APos.Side]);
        end else
          if LToCol = CColC then
          begin
            //Log.Append(Format('** Castling on queen side: %s', [AMove]));
            MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], CDTCD[APos.Side], CATCD[APos.Side]);
          end else
          begin
            Log.Append(Format('** Impossible move: %s', [MoveToStr(AMove)]));
            Exit(FALSE);
          end;
      end;
    
    APos.Roque[APos.Side].KingRookCol := CNil;
    APos.Roque[APos.Side].QueenRookCol := CNil;
    APos.KingSquare[APos.Side] := CIdxToSqr[LTo];
  end;
  
  { Si la pièce déplacée est une tour... }
  if LPieceType = ptRook then
    with APos.Roque[APos.Side] do
      if LFrCol = KingRookCol then
        KingRookCol := CNil
      else
        if LFrCol = QueenRookCol then
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
  if (LPieceType = ptWhitePawn) or (LPieceType = ptBlackPawn) then
  begin
    { Promotion. }
    if mtPromo in LMoveType then
      if mtKPromo in LMoveType then
      begin
        SwitchOffIdx(APos.Pawns, LFr);
        SwitchOnIdx(APos.Knights, LFr);
        LPieceType := ptKnight;
      end else
      if mtBPromo in LMoveType then
      begin
        SwitchOffIdx(APos.Pawns, LFr);
        SwitchOnIdx(APos.Bishops, LFr);
        LPieceType := ptBishop;
      end else
      if mtRPromo in LMoveType then
      begin
        SwitchOffIdx(APos.Pawns, LFr);
        SwitchOnIdx(APos.Rooks, LFr);
        LPieceType := ptRook;
      end else
      begin
        SwitchOffIdx(APos.Pawns, LFr);
        SwitchOnIdx(APos.Queens, LFr);
        LPieceType := ptQueen;
      end
    else
      { Prise en passant. }
      if LTo = APos.EnPassant then
      begin
        LEnPassantCapture := ToIndex(LToCol, LFrRow);
        SwitchOffIdx(APos.Pawns, LEnPassantCapture);
        SwitchOffIdx(APos.Pieces[not APos.Side], LEnPassantCapture);
      end;
  end;
  
  if ((LPieceType = ptWhitePawn) or (LPieceType = ptBlackPawn))
  and (Abs(LToRow - LFrRow) = 2) then
    APos.EnPassant := ToIndex(LFrCol, LFrRow + (LToRow - LFrRow) div 2)
  else
    APos.EnPassant := CNil;
  
  { Déplacement de la pièce. }
  case LPieceType of
    ptWhitePawn,
    ptBlackPawn: MovePieceIdx(APos.Pawns,   APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptRook:      MovePieceIdx(APos.Rooks,   APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptKnight:    MovePieceIdx(APos.Knights, APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptBishop:    MovePieceIdx(APos.Bishops, APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptQueen:     MovePieceIdx(APos.Queens,  APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptKing:      MovePieceIdx(APos.Kings,   APos.Pieces[APos.Side], LFr, LTo, LPreserve);
  end;
  { Changement du trait. }
  APos.Side := not APos.Side;
end;

function DoMove(var APos: TPosition; const AMove: string): boolean;
var
  LFr, LTo, LFrCol, LToCol, LFrRow, LToRow, LEnPassantCapture: integer;
  LType: TPieceType;
  LPreserve: boolean;
begin
  result := TRUE;
  
  { Index des cases de départ et d'arrivée. }
  LFr := DecodeSquareName(Copy(AMove, 1, 2));
  LTo := DecodeSquareName(Copy(AMove, 3, 2));
  
  Assert(IsOnIdx(APos.Pieces[APos.Side], LFr));
  
  if      IsOnIdx(APos.Pawns,   LFr) then if APos.Side then LType := ptBlackPawn else LType := ptWhitePawn
  else if IsOnIdx(APos.Rooks,   LFr) then LType := ptRook
  else if IsOnIdx(APos.Knights, LFr) then LType := ptKnight
  else if IsOnIdx(APos.Bishops, LFr) then LType := ptBishop
  else if IsOnIdx(APos.Queens,  LFr) then LType := ptQueen
  else if IsOnIdx(APos.Kings,   LFr) then LType := ptKing
  else
    Assert(FALSE);

  LFrCol := LFr mod 8;
  LToCol := LTo mod 8;
  LFrRow := LFr div 8;
  LToRow := LTo div 8;
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
        LPreserve := LFrCol = CATCR[APos.Side] mod 8;
      end else
        if LToCol = APos.Roque[APos.Side].QueenRookCol then
        begin
          Log.Append(Format('** Castling on A side: %s', [AMove]));
          MovePieceIdx(APos.Rooks, APos.Pieces[APos.Side], LTo, CATCD[APos.Side]);
          LTo := ToIndex(CColC, LToRow);
          LPreserve := LFrCol = CATCD[APos.Side] mod 8;
        end else
        begin
          Log.Append(Format('** Impossible move: %s', [AMove]));
          Exit(FALSE);
        end;
    end else
      if Abs(LToCol - LFrCol) = 2 then
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
      if LFrCol = KingRookCol then
        KingRookCol := CNil
      else
      if LFrCol = QueenRookCol then
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
    if IsPromotion(APos, AMove) then
      if (Length(AMove) = 5) then
        case AMove[5] of
          'n':
            begin
              SwitchOffIdx(APos.Pawns, LFr);
              SwitchOnIdx(APos.Knights, LFr);
              LType := ptKnight;
            end;
          'b':
            begin
              SwitchOffIdx(APos.Pawns, LFr);
              SwitchOnIdx(APos.Bishops, LFr);
              LType := ptBishop;
            end;
          'r':
            begin
              SwitchOffIdx(APos.Pawns, LFr);
              SwitchOnIdx(APos.Rooks, LFr);
              LType := ptRook;
            end;
          'q':
            begin
              SwitchOffIdx(APos.Pawns, LFr);
              SwitchOnIdx(APos.Queens, LFr);
              LType := ptQueen;
            end;
          else
            Log.Append(Format('** Unexpected value: %s', [AMove[5]]));    
        end
      else
      begin
        SwitchOffIdx(APos.Pawns, LFr);
        SwitchOnIdx(APos.Queens, LFr);
        LType := ptQueen;
      end;
    
    { Prise en passant. }
    if LTo = APos.EnPassant then
    begin
      LEnPassantCapture := ToIndex(LToCol, LFrRow);
      SwitchOffIdx(APos.Pawns, LEnPassantCapture);
      SwitchOffIdx(APos.Pieces[not APos.Side], LEnPassantCapture);
    end;
  end;
  
  if ((LType = ptWhitePawn) or (LType = ptBlackPawn))
  and (Abs(LToRow - LFrRow) = 2) then
    APos.EnPassant := ToIndex(LFrCol, LFrRow + (LToRow - LFrRow) div 2)
  else
    APos.EnPassant := CNil;
  
  { Déplacement de la pièce. }
  case LType of
    ptWhitePawn,
    ptBlackPawn: MovePieceIdx(APos.Pawns,   APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptRook:      MovePieceIdx(APos.Rooks,   APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptKnight:    MovePieceIdx(APos.Knights, APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptBishop:    MovePieceIdx(APos.Bishops, APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptQueen:     MovePieceIdx(APos.Queens,  APos.Pieces[APos.Side], LFr, LTo, LPreserve);
    ptKing:      MovePieceIdx(APos.Kings,   APos.Pieces[APos.Side], LFr, LTo, LPreserve);
  end;
  { Changement du trait. }
  APos.Side := not APos.Side;
end;

function IsPromotion(const APos: TPosition; const AMove: string): boolean;
var
  LFr, LTo: integer;
begin
  LFr := DecodeSquareName(Copy(AMove, 1, 2));
  LTo := DecodeSquareName(Copy(AMove, 3, 2)) div 8;
  result := IsOnIdx(APos.Pawns, LFr) and (
    not APos.Side and (LTo = CRow8)
    or  APos.Side and (LTo = CRow1)
  );
end;

function IsCastling(const APos: TPosition; const AMove: TMove): boolean;
var
  LFr, LTo: integer;
  LBlanc, LNoir: TBoard;
begin
  LBlanc := APos.Pieces[FALSE];
  LNoir := APos.Pieces[TRUE];
  DecodeMove(AMove, LFr, LTo);
  result :=
    (IsOnIdx(LBlanc, LFr) and IsOnIdx(LBlanc, LTo)) or
    (IsOnIdx(LNoir,  LFr) and IsOnIdx(LNoir,  LTo));
  if result then
    Log.Append(Format('** Castling move: %s', [MoveToStr(AMove)]));
end;

procedure RenameCastling(var AMove: TMove);
var
  LFr, LTo, LFrRow, LToRow, LToCol: integer;
  LOldName: string;
begin
  DecodeMove(AMove, LFr, LTo);
  Assert((LFr >= 0) and (LFr <= 63) and (LTo >= 0) and (LTo <= 63));
  LOldName := Concat(CSqrToStr[LFr], CSqrToStr[LTo]);
  LFrRow := LFr div 8;
  LToRow := LTo div 8;
  Assert((LToRow = LFrRow) and ((LFrRow = CRow1) or (LFrRow = CRow8)));
  if LTo mod 8 > LFr mod 8 then
    LToCol := CColG
  else
    LToCol := CColC;
  LTo := 8 * LToRow + LToCol;
  AMove := EncodeMove(LFr, LTo, ptKing, [mtCastling]);
  Log.Append(Format('** Reformulate %s to %s', [LOldName, Concat(CSqrToStr[LFr], CSqrToStr[LTo])]));
end;

end.
