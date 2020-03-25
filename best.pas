
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
  SysUtils, Move, Moves, Castling, Board, Tables, Log, History, Sort;

const
  CInfinite = 99999;
  CPieceValue: array[ptWhitePawn..ptQueen] of integer = (100, 100, 500, 320, 330, 900);

(*
var
  LEndTime: cardinal;
*)

function GetMaterialValue(const APos: TPosition): integer;
var
  LIndex, LSign: integer;
  LPiece: TPieceType;
  LWhitePieces, LBlackPieces, LSquare: TBoard;
begin
  if (APos.Pieces[APos.SideToMove] and APos.Kings) = 0 then
    Exit(-1 * CInfinite)
  else if (APos.Pieces[not APos.SideToMove] and APos.Kings) = 0 then
    Exit(CInfinite);
  result := 0;
  LWhitePieces := APos.Pieces[FALSE];
  LBlackPieces := APos.Pieces[TRUE];
  for LIndex := A1 to H8 do
  begin
    LSquare := CIndexToSquare[LIndex];
    if IsOn(LWhitePieces, LSquare) then
      LSign := 1
    else if IsOn(LBlackPieces, LSquare) then
      LSign := -1
    else
      Continue;
    LPiece := PieceTypeIdx(APos, LIndex);
    if LPiece <> ptKing then
      Inc(result, CPieceValue[LPiece] * LSign);
  end;
  if APos.SideToMove then
    result := -1 * result;
end;

function IsLegal(const APos: TPosition; const AMove: integer): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  if TryDoMove(LPos, MoveToStr(AMove)) then
  begin
    result := TRUE;
    LPos.SideToMove := not LPos.SideToMove;
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
  if TryDoMove(p, MoveToStr(AMove)) then
  begin
    GenMoves(p, m, c);
    result := High(integer);
    for i := 0 to Pred(c) do
    begin
      pp := p;
      if TryDoMove(pp, MoveToStr(m[i])) then
      begin
        GenMoves(pp, mm, cc);
        LMax := Low(integer);
        for ii := 0 to Pred(cc) do
        begin
          ppp := pp;
          if TryDoMove(ppp, MoveToStr(mm[ii])) then
          begin
            ppp.SideToMove := not ppp.SideToMove;
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

function Reverse(const AMove: string): string;
begin
  result := Concat(Copy(AMove, 3, 2), Copy(AMove, 1, 2));
end;

function PositionalEval(const APos: TPosition; const AMove: integer): integer;
begin
  result := 0;
end;
(*
var
  LPos: TPosition;
  LBonusRoque: integer;
  LMalusRepetition, LMalusAnnulation: integer;
  LBonusPiece: integer = -2;
  LTypePiece, LTypeCapture: TWidePieceType;
  LCaptureValue: integer;
  LFromSquare, LToSquare: integer;
  LTP: TPieceType;
  LTC: TMoveType;
  LBonusEnPassant, LBonusCapture: integer;
  LBonusMenaceRoi: integer;
  LBonusEchec: integer;
  LProtections: integer;
  LMalusPiece: integer;
begin
  DecodeMove(AMove, LFromSquare, LToSquare, LTP, LTC);
  
  LPos := APos;
  LBonusRoque := Ord(IsCastling(LPos, AMove)); if LBonusRoque = 1 then Assert(LTC = mtCastling);
  LMalusRepetition := Ord(MoveToStr(AMove) = PreviousPreviousMove);
  LMalusAnnulation := Ord(MoveToStr(AMove) = Reverse(PreviousMove));
  LTypePiece := PieceTypeIdx(LPos, StartIndex(AMove)); Assert(LTypePiece = LTP);
  LTypeCapture := PieceTypeIdx(LPos, TargetIndex(AMove));
  if LTypeCapture = ptNil then
  begin
    if LTC = mtEnPassant then
      LCaptureValue := CPieceValue[ptWhitePawn]
    else
      LCaptureValue := 0;
  end else
    LCaptureValue := CPieceValue[LTypeCapture];
  result := Low(integer);
  if not TryDoMove(LPos, MoveToStr(AMove)) then
    Exit;
  LBonusEchec := Ord(IsCheck(LPos));
  LPos.SideToMove := not LPos.SideToMove;
  LBonusPiece := 0;
  LBonusEnPassant := Ord(LTC = mtEnPassant);
  if LTC = mtCapture then
  begin
    if (LCaptureValue = CPieceValue[LTypePiece])
    or (LCaptureValue = CPieceValue[LTypePiece] - 10) then
      LBonusCapture := 1
    else if LCaptureValue > CPieceValue[LTypePiece] then
      LBonusCapture := 2
    else
      LBonusCapture := 0;
  end else
    LBonusCapture := 0;
  LBonusMenaceRoi := Ord((CTargets[LTP, LToSquare] and LPos.KingSquare[not LPos.SideToMove]) <> 0);
  LProtections := GetProtectionsCount(LPos);
  case LTypePiece of
    ptWhitePawn, ptBlackPawn: LMalusPiece := 0;
    ptKnight, ptBishop: LMalusPiece := 1; 
    ptRook, ptQueen: LMalusPiece := 2;
    ptKing: LMalusPiece := 3;
  end;
  
  result :=
    0
    + LBonusCapture
    + LBonusMenaceRoi
    + LBonusRoque
    + LBonusEnPassant
    + LBonusPiece
    - LMalusRepetition
    - LMalusAnnulation
    + LBonusEchec
    + LProtections
    - LMalusPiece;
   
  Log.Append(
    Format(
      '%s %d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
      [
        MoveToStr(AMove),
        LBonusCapture,
        LBonusMenaceRoi,
        LBonusRoque,
        LBonusEnPassant,
        LBonusPiece,
        LMalusRepetition,
        LMalusAnnulation,
        LBonusEchec,
        LProtections,
        LMalusPiece,
        result
      ]
    ),
    TRUE
  );
end;
*)

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
  (*
  LEndTime := GetTickCount64 + ATime;
  *)
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
  LMove := LListe[0];
  if IsCastling(APos, LMove) and not AVariant then
  begin
    Assert(((LMove and $FF00) shr 8) mod 8 = CColE);
    RenameCastlingMove(LMove);
  end;
  result := MoveToStr(LMove);
  if IsPromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
