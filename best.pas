
{**
  @abstract(Recherche du meilleur coup.)
  Recherche du meilleur coup.
}

unit Best;

interface

uses
  Chess;

function GetBestMove(const APos: TPosition; const AVariant: boolean; const ATime: integer): string;

var
  LTempMove: string;
  
implementation

uses
  SysUtils, Move, Moves, Castling, Board, Tables, Log, History;

procedure Swap(var AArray: array of integer; const i: integer);
var
  j: integer;
begin
  j := AArray[i];
  AArray[i] := AArray[i + 1];
  AArray[i + 1] := j;
end;

procedure SortMoves(var AMoves, AValues: array of integer; const n: integer);
var
  LIdx: integer;
  LDone: boolean;
begin
  repeat
    LDone := TRUE;
    for LIdx := 0 to n - 2 do
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

function GetMaterialValue(const APos: TPosition): integer;
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
      LSgn := +1
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
  if DoMove(LPos, MoveToStr(AMove)) then
  begin
    result := TRUE;
    LPos.Side := not LPos.Side;
    if IsCheck(LPos) then
      result := FALSE;
  end else
    result := FALSE;
end;

function MaterialEval(const APos: TPosition; const AMove: integer): integer;
var
  m, mm: array[0..199] of integer;
  p, pp, ppp: TPosition;
  i, ii: integer;
  c, cc: integer;
  LRes, LMax: integer;
begin
  p := APos;
  result := Low(integer);
  if DoMove(p, MoveToStr(AMove)) then
  begin
    GenMoves(p, m, c);
    result := High(integer);
    for i := 0 to Pred(c) do
    begin
      pp := p;
      if DoMove(pp, MoveToStr(m[i])) then
      begin
        GenMoves(pp, mm, cc);
        LMax := Low(integer);
        for ii := 0 to Pred(cc) do
        begin
          ppp := pp;
          if DoMove(ppp, MoveToStr(mm[ii])) then
          begin
            ppp.Side := not ppp.Side;
            if IsCheck(ppp) then
              LRes := -1 * CInfinite
            else
              LRes := GetMaterialValue(ppp);
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

function PositionalEval(const APos: TPosition; const AMove: integer): integer;
var
  LFrom, LTo: integer;
  LPieceType: TPieceType;
  LMoveType: TMoveType;
  LPos: TPosition;
  LProtections: integer;
begin
  result := 0;
  DecodeMove(AMove, LFrom, LTo, LPieceType, LMoveType);
  LPos := APos;
  if DoMove(LPos, MoveToStr(AMove)) then
  begin
    LPos.Side := not LPos.Side;
    LProtections := GetProtectionsCount(LPos);
    Inc(result, LProtections);
  end;
end;

function CountBestMoves(const ANotes: array of integer; const ALim: integer): integer;
begin
  result := 1;
  while (result < ALim) and (ANotes[result] = ANotes[0]) do
    Inc(result);
end;

function GetBestMove(const APos: TPosition; const AVariant: boolean; const ATime: integer): string;
var
  LListe, LEval: array[0..99] of integer;
  n, i, LMove: integer;
begin
  result := '0000';
  LTempMove := 'a1a1';
  LEndTime := GetTickCount64 + ATime;
  Log.Append(DecodePosition(APos), TRUE);
  
  GenMoves(APos, LListe, n);
  GenCastling(APos, LListe, n);
  
  for i := 0 to Pred(n) do
    LEval[i] := Ord(IsLegal(APos, LListe[i]));
  SortMoves(LListe, LEval, n);
  Log.Append(LListe, n);
  
  n := CountBestMoves(LEval, n);
  LTempMove := MoveToStr(LListe[Random(n)]);
  if IsPromotion(APos, LTempMove) then
    LTempMove := Concat(LTempMove, 'q');
{$IFDEF RANDOM_MOVER}
  Exit(LTempMove);
{$ENDIF}
  for i := 0 to Pred(n) do
    LEval[i] := MaterialEval(APos, LListe[i]);
  SortMoves(LListe, LEval, n);
  Log.Append(LListe, LEval, n);
  n := CountBestMoves(LEval, n);
  LTempMove := MoveToStr(LListe[Random(n)]);
  if IsPromotion(APos, LTempMove) then
    LTempMove := Concat(LTempMove, 'q');
  if n = 1 then
    Exit(LTempMove);
  for i := 0 to Pred(n) do
    LEval[i] := PositionalEval(APos, LListe[i]);
  SortMoves(LListe, LEval, n);
  Log.Append(LListe, LEval, n);
  n := CountBestMoves(LEval, n);
  LMove := LListe[Random(n)];
  if IsCastling(APos, LMove) and not AVariant then
    RenameCastlingMove(LMove);

  result := MoveToStr(LMove);
  if IsPromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
