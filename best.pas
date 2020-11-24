
{**
  @abstract(Recherche du meilleur coup.)
  Recherche du meilleur coup.
}

unit Best;

interface

uses
  Chess;

function GetBestMove(const APos: TPosition; const AVariant: boolean; const ATime: integer; var AMove: string; const ARandomMove: boolean = FALSE): string;
  
implementation

uses
  SysUtils, Move, Moves, Castling, Board, Tables, Log, History;

procedure Swap(var AArr: array of integer; const AIdx: integer);
var
  LAux: integer;
begin
  LAux := AArr[AIdx];
  AArr[AIdx] := AArr[AIdx + 1];
  AArr[AIdx + 1] := LAux;
end;

procedure Sort(var AMoves, AValues: array of integer; const ACount: integer);
var
  LIdx: integer;
  LDone: boolean;
begin
  repeat
    LDone := TRUE;
    for LIdx := 0 to ACount - 2 do
      if AValues[LIdx] < AValues[LIdx + 1] then
      begin
        Swap(AMoves, LIdx);
        Swap(AValues, LIdx);
        LDone := FALSE;
      end;
  until LDone;
end;

const
  CInfinite = 99999;
  CPieceValue: array[ptWhitePawn..ptQueen] of integer = (100, 100, 500, 320, 330, 900);
  
var
  LEndTime: cardinal;

function MaterialAdvantage(const APos: TPosition): integer;
var
  LIdx, LSgn: integer;
  LPiece: TPieceType;
  LWPieces, LBPieces, LSqr: TBoard;
begin
  if (APos.Pieces[APos.Side] and APos.Kings) = 0 then
    Exit(-1 * CInfinite);
  if (APos.Pieces[not APos.Side] and APos.Kings) = 0 then
    Exit(CInfinite);
  result := 0;
  LWPieces := APos.Pieces[FALSE];
  LBPieces := APos.Pieces[TRUE];
  for LIdx := A1 to H8 do
  begin
    LSqr := CIdxToSqr[LIdx];
    if IsOn(LWPieces, LSqr) then
      LSgn :=  1
    else if IsOn(LBPieces, LSqr) then
      LSgn := -1
    else
      Continue;
    LPiece := PieceTypeIdx(APos, LIdx);
    if LPiece <> ptKing then
      Inc(result, CPieceValue[LPiece] * LSgn);
  end;
  if APos.Side then
    result := -1 * result;
end;

function IsLegal(const APos: TPosition; const AMove: integer): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  if DoMove(LPos, AMove) then
  begin
    result := TRUE;
    LPos.Side := not LPos.Side;
    if IsCheck(LPos) then
      result := FALSE;
  end else
    result := FALSE;
end;

function Eval1(const APos: TPosition; const AMove: integer): integer;
var
  LMov: array[1..2, 0..199] of integer;
  LPos: array[1..3] of TPosition;
  LCnt: array[1..2] of integer;
  LRes, LMax: integer;
  i, j: integer;
begin
  LPos[1] := APos;
  result := Low(integer);
  if DoMove(LPos[1], AMove) then
  begin
    GenMoves(LPos[1], LMov[1], LCnt[1]);
    result := High(integer);
    for i := 0 to Pred(LCnt[1]) do
    begin
      LPos[2] := LPos[1];
      if DoMove(LPos[2], LMov[1][i]) then
      begin
        GenMoves(LPos[2], LMov[2], LCnt[2]);
        LMax := Low(integer);
        for j := 0 to Pred(LCnt[2]) do
        begin
          LPos[3] := LPos[2];
          if DoMove(LPos[3], LMov[2][j]) then
          begin
            LPos[3].Side := not LPos[3].Side;
            if IsCheck(LPos[3]) then
              LRes := -1 * CInfinite
            else
              LRes := MaterialAdvantage(LPos[3]);
          end else
            Continue;
          if LRes > LMax then
          begin
            LMax := LRes;
           if LMax = CInfinite then
             Break;
          end;
        end;
        if LMax < result then
          result := LMax;
      end else
        Continue;
    end;
  end;
end;

function Eval2(const APos: TPosition; const AMove: integer): integer;
var
  LFrom, LTo: integer;
  LPieceType: TPieceType;
  LMoveType: TMoveTypeSet;
  LPos: TPosition;
  LCastling,
  LEnPassant,
  LCheck,
  LThreatensKing,
  LTargetsNumber,
  LProtections,
  LAttacks: integer;
begin
  DecodeMove(AMove, LFrom, LTo, LPieceType, LMoveType);
  LPos := APos;
  
  LCastling := Ord(mtCastling in LMoveType);
  LEnPassant := Ord(mtEnPassant in LMoveType);
  
  if DoMove(LPos, AMove) then
  begin
    LCheck := Ord(IsCheck(LPos));
    LPos.Side := not LPos.Side;
    
    LProtections := GetProtectionsCount(LPos);
    LAttacks := GetAttacksCount(LPos);
    
    LThreatensKing := PopCnt(QWord((CTargets[LPieceType, LTo] and (LPos.KingSquare[not LPos.Side] or (LPos.Pieces[not LPos.Side] and LPos.Queens)))));
    
    if LPieceType in [ptKnight, ptBishop] then
      LTargetsNumber := PopCnt(QWord(CTargets[LPieceType, LTo]))
    else
      LTargetsNumber := 0;
  end else
    Assert(FALSE, 'Cannot do move');
  
  LCastling := 10 * LCastling;
  LEnPassant := 10 * LEnPassant;
  LCheck := 10 * LCheck;
  LThreatensKing := 10 * LThreatensKing;
  
  {$IFDEF DEBUG}
  Log.Append(Format(
    '  %s Castling %0.2d EnPassant %0.2d Check %0.2d Protections %0.2d Attacks %0.2d Threatens %0.2d Targets %0.2d',
    [MoveToStr(AMove), LCastling, LEnPassant, LCheck, LProtections, LAttacks, LThreatensKing, LTargetsNumber]
  ), TRUE);
  {$ENDIF}
  
  result := LCastling + LEnPassant + LCheck + LProtections + LAttacks + LThreatensKing + LTargetsNumber;
end;

function CountBestMoves(const ANotes: array of integer; const AMax: integer): integer;
begin
  result := 1;
  while (result < AMax) and (ANotes[result] = ANotes[0]) do
    Inc(result);
end;

function GetBestMove(const APos: TPosition; const AVariant: boolean; const ATime: integer; var AMove: string; const ARandomMove: boolean): string;
var
  LList, LEval: array[0..199] of integer;
  LCount, LMove, i: integer;
begin  
  LEndTime := GetTickCount64 + ATime;
  Log.Append(Concat('** Position: ', DecodePosition(APos)), TRUE);
  Log.Append(Format('** Time available: %d ms', [ATime]), TRUE);
  
  GenMoves(APos, LList, LCount);
  GenCastling(APos, LList, LCount);
  
  { I }
  for i := 0 to Pred(LCount) do
    LEval[i] := Ord(IsLegal(APos, LList[i]));
  Sort(LList, LEval, LCount);
  LCount := CountBestMoves(LEval, LCount);
  LMove := LList[Random(LCount)];
  if IsCastling(APos, LMove) and not AVariant then
    RenameCastling(LMove);
  AMove := MoveToStr(LMove);
  if IsPromotion(APos, AMove) then
    AMove := Concat(AMove, 'q');

  if ARandomMove then
  begin
    result := AMove;
    Exit;
  end;
  
  { II }
  for i := 0 to Pred(LCount) do
    LEval[i] := Eval1(APos, LList[i]);
  Sort(LList, LEval, LCount);
  Log.Append(LList, LEval, LCount);
  LCount := CountBestMoves(LEval, LCount);
  LMove := LList[Random(LCount)];
  if IsCastling(APos, LMove) and not AVariant then
    RenameCastling(LMove);
  AMove := MoveToStr(LMove);
  if IsPromotion(APos, AMove) then
    AMove := Concat(AMove, 'q');

  { III }
  for i := 0 to Pred(LCount) do
    LEval[i] := Eval2(APos, LList[i]);
  Sort(LList, LEval, LCount);
  Log.Append(LList, LEval, LCount);
  LCount := CountBestMoves(LEval, LCount);
  LMove := LList[Random(LCount)];
  if IsCastling(APos, LMove) and not AVariant then
    RenameCastling(LMove);
  result := MoveToStr(LMove);
  if IsPromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
