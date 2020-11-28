
{**
  @abstract(Recherche du meilleur coup.)
  Recherche du meilleur coup.
}

unit Best;

interface

uses
  Chess;

function GetBestMove(const APos: TPosition; const AFrc: boolean; const ATime: integer; var AMove: string; const ARandMove: boolean = FALSE): string;
  
implementation

uses
  SysUtils, Move, Moves, Castling, Board, Tables, Log, History;

const
  CInfinite = 99999;
  CValue: array[ptWhitePawn..ptQueen] of integer = (100, 100, 500, 320, 330, 900);
  
var
  LEndTime: cardinal;

type
  TEvalFunc = function(const APos: TPosition; const AMove: TMove): integer;

function Eval0(const APos: TPosition; const AMove: TMove): integer;
var
  LPos: TPosition;
begin
  LPos := APos;
  result := 0;
  if DoMove(LPos, AMove) then
  begin
    LPos.Side := not LPos.Side;
    result := Ord(not IsCheck(LPos));
  end;
end;

function Eval1(const APos: TPosition; const AMove: TMove): integer;

  function MaterialAdvantage(const APos: TPosition): integer;
  var
    LIdx: integer;
    LType: TPieceType;
    LPieces: TBoard;
  begin
    if (APos.Pieces[APos.Side] and APos.Kings) = 0 then
      Exit(-1 * CInfinite);
    
    if (APos.Pieces[not APos.Side] and APos.Kings) = 0 then
      Exit(CInfinite);
    
    result := 0;
    
    LPieces := APos.Pieces[FALSE] and not APos.Kings;
    while LPieces <> 0 do
    begin
      LIdx := BsfQWord(QWord(LPieces));
      LType := PieceTypeIdx(APos, LIdx);
      Inc(result, CValue[LType]);
      LPieces := LPieces and not CIdxToSqr[LIdx];
    end;
    
    LPieces := APos.Pieces[TRUE] and not APos.Kings;
    while LPieces <> 0 do
    begin
      LIdx := BsfQWord(QWord(LPieces));
      LType := PieceTypeIdx(APos, LIdx);
      Dec(result, CValue[LType]);
      LPieces := LPieces and not CIdxToSqr[LIdx];
    end;

    if APos.Side then
      result := -1 * result;
  end;

var
  LMov: array[1..2, 0..199] of TMove;
  LPos: array[1..3] of TPosition;
  LCnt: array[1..2] of integer;
  LRes, LMax: integer;
  i, j: integer;
begin
  Initialize(LMov);
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

function Eval2(const APos: TPosition; const AMove: TMove): integer;
var
  LFr, LTo: integer;
  LPieceType: TPieceType;
  LMoveType: TMoveTypeSet;
  LPos: TPosition;
  LCastling,
  LEnPassant,
  LCheck,
  LMajorTargets,
  LTargets,
  LProtections,
  LAttacks: integer;
  LPieces: TBoard;
begin
  DecodeMove(AMove, LFr, LTo, LPieceType, LMoveType);
  LCastling := Ord(mtCastling in LMoveType);
  LEnPassant := Ord(mtEnPassant in LMoveType);
  
  LPos := APos;
  if DoMove(LPos, AMove) then
  begin
    LCheck := Ord(IsCheck(LPos));
    LPos.Side := not LPos.Side;
    
    LProtections := GetProtectionsCount(LPos);
    LAttacks := GetAttacksCount(LPos);
    
    LPieces := LPos.Pieces[LPos.Side];
    LMajorTargets := 0;
    LTargets := 0;
    while LPieces <> 0 do
    begin
      LFr := BsfQWord(QWord(LPieces));
      LPieceType := PieceTypeIdx(LPos, LFr);
      Inc(LMajorTargets, PopCnt(QWord(
        CTargets[LPieceType, LFr] and LPos.Pieces[not LPos.Side] and (LPos.Kings or LPos.Queens)
      )));
      if LPieceType in [ptKnight, ptBishop, ptRook, ptQueen] then
        Inc(LTargets, PopCnt(QWord(CTargets[LPieceType, LFr] and not LPos.Pieces[LPos.Side])));
      LPieces := LPieces and not CIdxToSqr[LFr];
    end;
  end else
    Assert(FALSE, 'Cannot do move');
  
  LCastling := 10 * LCastling;
  LEnPassant := 10 * LEnPassant;
  LCheck := 10 * LCheck;
  LMajorTargets := 2 * LMajorTargets;
  
  {$IFDEF DEBUG}
  Log.Append(Format(
    '%-5s Castling %0.2d EnPassant %0.2d Check %0.2d Protections %0.2d Attacks %0.2d MajorTargets %0.2d Targets %0.2d',
    [MoveToStr(AMove), LCastling, LEnPassant, LCheck, LProtections, LAttacks, LMajorTargets, LTargets]
  ), TRUE);
  {$ENDIF}
  
  result := LCastling + LEnPassant + LCheck + LProtections + LAttacks + LMajorTargets + LTargets;
end;

function GetBestMove(const APos: TPosition; const AFrc: boolean; const ATime: integer; var AMove: string; const ARandMove: boolean): string;
var
  LList: array[0..199] of TMove;
  LEval: array[0..199] of integer;
  LCount: integer;
  
  procedure Evaluate(AFunc: TEvalFunc);

    procedure Sort(var AMoves: array of TMove; var AValues: array of integer; const ACount: integer);
    
      procedure Swap(var AArr: array of integer; const AIdx: integer);
      var
        LAux: integer;
      begin
        LAux := AArr[AIdx];
        AArr[AIdx] := AArr[AIdx + 1];
        AArr[AIdx + 1] := LAux;
      end;

      procedure Swap(var AArr: array of TMove; const AIdx: integer);
      var
        LAux: TMove;
      begin
        LAux := AArr[AIdx];
        AArr[AIdx] := AArr[AIdx + 1];
        AArr[AIdx + 1] := LAux;
      end;
    
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
    
    function CountBestMoves(const AValues: array of integer; const AStop: integer): integer;
    begin
      result := 1;
      while (result < AStop) and (AValues[result] = AValues[0]) do
        Inc(result);
    end;
  
  var
    i: integer;
    LMove: TMove;
  begin
    for i := 0 to Pred(LCount) do
      LEval[i] := AFunc(APos, LList[i]);
    Sort(LList, LEval, LCount);
    Log.Append(LList, LEval, LCount);
    LCount := CountBestMoves(LEval, LCount);
    LMove := LList[Random(LCount)];
    if IsCastling(APos, LMove) and not AFrc then
      RenameCastling(LMove);
    AMove := MoveToStr(LMove);
  end;

begin
  Initialize(LList);
  LEndTime := GetTickCount64 + ATime;
  Log.Append(Concat('** Position: ', DecodePosition(APos)), TRUE);
  Log.Append(Format('** Time available: %d ms', [ATime]), TRUE);
  GenMoves(APos, LList, LCount);
  GenCastling(APos, LList, LCount);
  Evaluate(@Eval0);
  if ARandMove then
    Exit(AMove);
  Evaluate(@Eval1);
  Evaluate(@Eval2);
  result := AMove;
end;

end.
