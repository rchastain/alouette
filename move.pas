
{**
  @abstract(Déplacement des pièces.)
  Complément de l'unité Chess.
}

unit Move;

interface

uses
  Board, Chess;

function PieceTypeIdx(const APos: TPosition; const AIdx: integer): TWidePieceType;
{** Met à jour la position en fonction d'un coup présumé légal. Renvoie FALSE si une impossibilité de jouer le coup est détectée. }
function TryDoMove(var APos: TPosition; const AMove: string): boolean;
function IsPromotion(const APos: TPosition; const AMove: string): boolean;
function IsCastling(const APos: TPosition; const AMove: integer): boolean;
procedure RenameCastlingMove(var ARoque: integer);

implementation

uses
  SysUtils, Tables, Log;

function PieceTypeIdx(const APos: TPosition; const AIdx: integer): TWidePieceType;
begin
  if      IsOnIdx(APos.Pawns,   AIdx) then if APos.SideToMove then result := ptBlackPawn else result := ptWhitePawn
  else if IsOnIdx(APos.Rooks,   AIdx) then result := ptRook
  else if IsOnIdx(APos.Knights, AIdx) then result := ptKnight
  else if IsOnIdx(APos.Bishops, AIdx) then result := ptBishop
  else if IsOnIdx(APos.Queens,  AIdx) then result := ptQueen
  else if IsOnIdx(APos.Kings,   AIdx) then result := ptKing
  else
    result := ptNil;
end;

function TryDoMove(var APos: TPosition; const AMove: string): boolean;
var
  LDep, LArr, LColDep, LColArr, LLigDep, LLigArr, LPris: integer;
  LType: TPieceType;
  LSuper: boolean;
begin
  result := TRUE;
  
  { Conversion de la chaîne en index des cases de départ et d'arrivée. L'index est un nombre de 0 à 63. }
  LDep := DecodeSquareName(Copy(AMove, 1, 2));
  LArr := DecodeSquareName(Copy(AMove, 3, 2));
  
  Assert(IsOnIdx(APos.Pieces[APos.SideToMove], LDep), 'Impossible de déterminer la couleur de la pièce.');
  
  if      IsOnIdx(APos.Pawns,   LDep) then if APos.SideToMove then LType := ptBlackPawn else LType := ptWhitePawn
  else if IsOnIdx(APos.Rooks,   LDep) then LType := ptRook
  else if IsOnIdx(APos.Knights, LDep) then LType := ptKnight
  else if IsOnIdx(APos.Bishops, LDep) then LType := ptBishop
  else if IsOnIdx(APos.Queens,  LDep) then LType := ptQueen
  else if IsOnIdx(APos.Kings,   LDep) then LType := ptKing
  else
    Assert(FALSE, 'Impossible de déterminer le type de la pièce.');

  LColDep := LDep mod 8;
  LColArr := LArr mod 8;
  LLigDep := LDep div 8;
  LLigArr := LArr div 8;
  LSuper := FALSE;
  
  { Si la pièce déplacée est un roi... }
  if LType = ptKing then
  begin
    if IsOnIdx(APos.Rooks and APos.Pieces[APos.SideToMove], LArr) then
    begin
      if LColArr = APos.Roque[APos.SideToMove].KingRookCol then
      begin
        Log.Append(Format('Roque côté H (%s).', [AMove]));
        MovePieceIdx(APos.Rooks, APos.Pieces[APos.SideToMove], LArr, CATCR[APos.SideToMove]);
        LArr := ToIndex(CColG, LLigArr);
        LSuper := LColDep = CATCR[APos.SideToMove] mod 8;
      end else
        if LColArr = APos.Roque[APos.SideToMove].QueenRookCol then
        begin
          Log.Append(Format('Roque côté A (%s).', [AMove]));
          MovePieceIdx(APos.Rooks, APos.Pieces[APos.SideToMove], LArr, CATCD[APos.SideToMove]);
          LArr := ToIndex(CColC, LLigArr);
          LSuper := LColDep = CATCD[APos.SideToMove] mod 8;
        end else
        begin
          Log.Append(Format('Impossible de rejouer %s.', [AMove]));
          Exit(FALSE);
        end;
    end else
      if Abs(LColArr - LColDep) = 2 then
      begin
        if LColArr = CColG then
        begin
          Log.Append(Format('Roque côté roi (%s).', [AMove]));
          MovePieceIdx(APos.Rooks, APos.Pieces[APos.SideToMove], CDTCR[APos.SideToMove], CATCR[APos.SideToMove]);
        end else
          if LColArr = CColC then
          begin
            Log.Append(Format('Roque côté dame (%s).', [AMove]));
            MovePieceIdx(APos.Rooks, APos.Pieces[APos.SideToMove], CDTCD[APos.SideToMove], CATCD[APos.SideToMove]);
          end else
          begin
            Log.Append(Format('Impossible de rejouer %s.', [AMove]));
            Exit(FALSE);
          end;
      end;
    
    APos.Roque[APos.SideToMove].KingRookCol := CNil;
    APos.Roque[APos.SideToMove].QueenRookCol := CNil;
    APos.KingSquare[APos.SideToMove] := CIndexToSquare[LArr];
  end;
  
  { Si la pièce déplacée est une tour... }
  if LType = ptRook then
    with APos.Roque[APos.SideToMove] do
      if LColDep = KingRookCol then
        KingRookCol := CNil
      else
      if LColDep = QueenRookCol then
        QueenRookCol := CNil;
  
  { S'il y a une pièce sur la case d'arrivée... }
  if IsOnIdx(APos.Pieces[not APos.SideToMove], LArr) then
  begin
    if IsOnIdx(APos.Rooks, LArr)
    and (LLigArr = CCastlingRow[not APos.SideToMove]) then
      with APos.Roque[not APos.SideToMove] do
        if (LColArr = KingRookCol) then
          KingRookCol := CNil
        else
        if LColArr = KingRookCol then
          KingRookCol := CNil;
        
    with APos do
    begin
      SwitchOffIdx(Pawns,   LArr);
      SwitchOffIdx(Rooks,   LArr);
      SwitchOffIdx(Knights, LArr);
      SwitchOffIdx(Bishops, LArr);
      SwitchOffIdx(Queens,  LArr);
      SwitchOffIdx(Kings,   LArr);
    end;
    SwitchOffIdx(APos.Pieces[not APos.SideToMove], LArr);
  end;
  
  { Si la pièce déplacée est un pion... }
  if (LType = ptWhitePawn) or (LType = ptBlackPawn) then
  begin
    { Promotion. }
    if (Length(AMove) = 4) and IsPromotion(APos, AMove) then
    begin
      SwitchOffIdx(APos.Pawns, LDep);
      SwitchOnIdx(APos.Queens, LDep);
      LType := ptQueen;
    end else
      if (Length(AMove) = 5) then
        case AMove[5] of
          'n':
            begin
              SwitchOffIdx(APos.Pawns, LDep);
              SwitchOnIdx(APos.Knights, LDep);
              LType := ptKnight;
            end;
          'b':
            begin
              SwitchOffIdx(APos.Pawns, LDep);
              SwitchOnIdx(APos.Bishops, LDep);
              LType := ptBishop;
            end;
          'r':
            begin
              SwitchOffIdx(APos.Pawns, LDep);
              SwitchOnIdx(APos.Rooks, LDep);
              LType := ptRook;
            end;
          'q':
            begin
              SwitchOffIdx(APos.Pawns, LDep);
              SwitchOnIdx(APos.Queens, LDep);
              LType := ptQueen;
            end;
          else
            Log.Append(Format('Valeur inattendue (%s).', [AMove[5]]));    
        end;
    
    { Prise en passant. }
    if LArr = APos.EnPassant then
    begin
      LPris := ToIndex(LColArr, LLigDep);
      SwitchOffIdx(APos.Pawns, LPris);
      SwitchOffIdx(APos.Pieces[not APos.SideToMove], LPris);
    end;
  end;
  
  if ((LType = ptWhitePawn) or (LType = ptBlackPawn)) and (Abs(LLigArr - LLigDep) = 2) then
    APos.EnPassant := ToIndex(LColDep, LLigDep + (LLigArr - LLigDep) div 2)
  else
    APos.EnPassant := CNil;
  
  { Déplacement de la pièce. }
  case LType of
    ptWhitePawn,
    ptBlackPawn: MovePieceIdx(APos.Pawns,   APos.Pieces[APos.SideToMove], LDep, LArr, LSuper);
    ptRook:      MovePieceIdx(APos.Rooks,   APos.Pieces[APos.SideToMove], LDep, LArr, LSuper);
    ptKnight:    MovePieceIdx(APos.Knights, APos.Pieces[APos.SideToMove], LDep, LArr, LSuper);
    ptBishop:    MovePieceIdx(APos.Bishops, APos.Pieces[APos.SideToMove], LDep, LArr, LSuper);
    ptQueen:     MovePieceIdx(APos.Queens,  APos.Pieces[APos.SideToMove], LDep, LArr, LSuper);
    ptKing:      MovePieceIdx(APos.Kings,   APos.Pieces[APos.SideToMove], LDep, LArr, LSuper);
  end;
  { Changement du trait. }
  APos.SideToMove := not APos.SideToMove;
end;

function IsPromotion(const APos: TPosition; const AMove: string): boolean;
var
  LDep, LArr: integer;
begin
  LDep := DecodeSquareName(Copy(AMove, 1, 2));
  LArr := DecodeSquareName(Copy(AMove, 3, 2)) div 8;
  result := IsOnIdx(APos.Pawns, LDep) and (
    not APos.SideToMove and (LArr = CRow8)
    or  APos.SideToMove and (LArr = CRow1)
  );
end;

function IsCastling(const APos: TPosition; const AMove: integer): boolean;
var
  LDep, LArr: integer;
  LBlanc, LNoir: TBoard;
begin
  LBlanc := APos.Pieces[FALSE];
  LNoir := APos.Pieces[TRUE];
  DecodeMove(AMove, LDep, LArr);
  result :=
    (IsOnIdx(LBlanc, LDep) and IsOnIdx(LBlanc, LArr)) or
    (IsOnIdx(LNoir,  LDep) and IsOnIdx(LNoir,  LArr));
  if result then
    Log.Append(Format('Roque détecté (%s).', [MoveToStr(AMove)]));
end;

procedure RenameCastlingMove(var ARoque: integer);
var
  LDep, LArr, LLigDep, LLigArr, LColArr: integer;
  LAncienNom: string;
begin
  DecodeMove(ARoque, LDep, LArr);
  Assert((LDep >= 0) and (LDep <= 63) and (LArr >= 0) and (LArr <= 63));
  LAncienNom := Concat(CSquareToStr[LDep], CSquareToStr[LArr]);
  LLigDep := LDep div 8;
  LLigArr := LArr div 8;
  Assert((LLigArr = LLigDep) and ((LLigDep = CRow1) or (LLigDep = CRow8)));
  if LArr mod 8 > LDep mod 8 then
    LColArr := CColG
  else
    LColArr := CColC;
  LArr := 8 * LLigArr + LColArr;
  ARoque := EncodeMove(LDep, LArr, ptKing);
  Log.Append(Format('Reformulé %s en %s.', [LAncienNom, Concat(CSquareToStr[LDep], CSquareToStr[LArr])]));
end;

end.
