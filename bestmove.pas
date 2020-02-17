
{**
  @abstract(Recherche du meilleur coup.)
  Recherche du meilleur coup.
}

unit BestMove;

interface

uses
  Chess;

function GetBestMove(const APos: TPosition; const AFrc: boolean; const ATime: integer): string;

var
  LTempMove: string;
  
implementation

uses
  SysUtils, Move, Moves, Castling, Board, Tables, Log, History, Sort;

const
  CInfinite = 99999;
  CPieceValue: array[PionBlanc..Dame] of integer = (100, 100, 500, 320, 330, 900);
  
var
  LEndTime: cardinal;

function Materiel(const APos: TPosition): integer;
var
  LIndex, LSign: integer;
  LPiece: TTypePiece;
  LWhitePieces, LBlackPieces, LSquare: TDamier;
begin
  if (APos.Pieces[APos.Trait] and APos.Rois) = 0 then
    Exit(-1 * CInfinite)
  else if (APos.Pieces[not APos.Trait] and APos.Rois) = 0 then
    Exit(CInfinite);
  result := 0;
  LWhitePieces := APos.Pieces[FALSE];
  LBlackPieces := APos.Pieces[TRUE];
  for LIndex := A1 to H8 do
  begin
    LSquare := CCaseIdx[LIndex];
    if EstAllumee(LWhitePieces, LSquare) then
      LSign := 1
    else if EstAllumee(LBlackPieces, LSquare) then
      LSign := -1
    else
      Continue;
    LPiece := TypePieceIdx(APos, LIndex);
    if LPiece <> Roi then
      Inc(result, CPieceValue[LPiece] * LSign);
  end;
  if APos.Trait then
    result := -1 * result;
end;

function IsLegal(const APos: TPosition; const AMove: integer): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  if FRejoue(LPos, NomCoup(AMove)) then
  begin
    result := TRUE;
    LPos.Trait := not LPos.Trait;
    if FEchec(LPos) then
      result := FALSE;
  end else
    result := FALSE;
end;

function MaterialEval(const APos: TPosition; const AMove: integer; out ACheckRisk: boolean): integer;
var
  m, mm: array[0..199] of integer;
  p, pp, ppp: TPosition;
  i, ii: integer;
  c, cc: integer;
  LRes, LMax: integer;
begin
  ACheckRisk := FALSE;
  p := APos;
  result := Low(integer);
  if FRejoue(p, NomCoup(AMove)) then
  begin
    FCoups(p, m, c);
    result := High(integer);
    for i := 0 to Pred(c) do
    begin
      pp := p;
      if FRejoue(pp, NomCoup(m[i])) then
      begin
        if FEchec(pp) then
          ACheckRisk := TRUE;
        FCoups(pp, mm, cc);
        LMax := Low(integer);
        for ii := 0 to Pred(cc) do
        begin
          ppp := pp;
          if FRejoue(ppp, NomCoup(mm[ii])) then
          begin
            ppp.Trait := not ppp.Trait;
            if FEchec(ppp) then
              LRes := -1 * CInfinite
            else
              LRes := Materiel(ppp);
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
var
  LPos: TPosition;
  LBonusRoque: integer;
  LMalusRepetition, LMalusAnnulation: integer;
  LBonusPiece: integer = -2;
  LTypePiece, LTypeCapture: TTypePieceLarge;
  LCaptureValue: integer;
  LFromSquare, LToSquare: integer;
  LTP: TTypePiece;
  LTC: TTypeCoup;
  LBonusEnPassant, LBonusCapture: integer;
  LBonusMenaceRoi: integer;
  LBonusEchec: integer;
  LProtections: integer;
  LMalusPiece: integer;
begin
  DecodeCoup(AMove, LFromSquare, LToSquare, LTP, LTC);
  
  LPos := APos;
  LBonusRoque := Ord(EstUnRoque(LPos, AMove)); if LBonusRoque = 1 then Assert(LTC = tcRoque);
  LMalusRepetition := Ord(NomCoup(AMove) = AvantDernier);
  LMalusAnnulation := Ord(NomCoup(AMove) = Reverse(Dernier));
  LTypePiece := TypePieceIdx(LPos, Depart(AMove)); Assert(LTypePiece = LTP);
  LTypeCapture := TypePieceIdx(LPos, Arrivee(AMove));
  if LTypeCapture = Neant then
  begin
    if LTC = tcEnPassant then
      LCaptureValue := CPieceValue[PionBlanc]
    else
      LCaptureValue := 0;
  end else
    LCaptureValue := CPieceValue[LTypeCapture];
  result := Low(integer);
  if not FRejoue(LPos, NomCoup(AMove)) then
    Exit;
  LBonusEchec := Ord(FEchec(LPos));
  LPos.Trait := not LPos.Trait;
  LBonusPiece := 0;
  LBonusEnPassant := Ord(LTC = tcEnPassant);
  if LTC = tcCapture then
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
  LBonusMenaceRoi := Ord((CCibles[LTP, LToSquare] and LPos.CaseRoi[not LPos.Trait]) <> 0);
  LProtections := FProtections(LPos);
  case LTypePiece of
    PionBlanc, PionNoir: LMalusPiece := 0;
    Cavalier, Fou: LMalusPiece := 1; 
    Tour, Dame: LMalusPiece := 2;
    Roi: LMalusPiece := 3;
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
   
  Log.Ajoute(
    Format(
      '%s %d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
      [
        NomCoup(AMove),
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

function CompteMeilleurs(const ANotes: array of integer; const ALim: integer): integer;
begin
  result := 1;
  while (result < ALim) and (ANotes[result] = ANotes[0]) do
    Inc(result);
end;

function GetBestMove(const APos: TPosition; const AFrc: boolean; const ATime: integer): string;
var
  LListe, LEval: array[0..99] of integer;
  n, i, LCoup: integer;
  LCheckRisk: boolean;
begin
  result := '0000';
  LTempMove := 'a1a1';
  LEndTime := GetTickCount64 + ATime;
  Log.Ajoute('', TRUE);
  Log.Ajoute(DecodePosition(APos), TRUE);
  FCoups(APos, LListe, n);
  FRoque(APos, LListe, n);
  
  for i := 0 to Pred(n) do
    LEval[i] := Ord(IsLegal(APos, LListe[i]));
  Trie(LListe, LEval, n);
  Log.Ajoute('', TRUE);
  Log.AjouteTable(LListe, n);
  
  n := CompteMeilleurs(LEval, n);
  LTempMove := NomCoup(LListe[Random(n)]);
  if EstUnePromotion(APos, LTempMove) then
    LTempMove := Concat(LTempMove, 'q');
{$IFDEF RANDOM_MOVER}
  Exit(LTempMove);
{$ENDIF}
  for i := 0 to Pred(n) do
  begin
    LEval[i] := MaterialEval(APos, LListe[i], LCheckRisk);
    Dec(LEval[i], Ord(LCheckRisk));
  end;
  Trie(LListe, LEval, n);
  (*
  LTempMove := NomCoup(LListe[0]);
  if EstUnePromotion(APos, LTempMove) then
    LTempMove := Concat(LTempMove, 'q');
  *)
  Log.Ajoute('', TRUE);
  Log.AjouteTable(LListe, LEval, n);
  
  n := CompteMeilleurs(LEval, n);
  LTempMove := NomCoup(LListe[Random(n)]);
  if EstUnePromotion(APos, LTempMove) then
    LTempMove := Concat(LTempMove, 'q');
  Log.Ajoute('', TRUE);
  for i := 0 to Pred(n) do
    LEval[i] := PositionalEval(APos, LListe[i]);
  
  Trie(LListe, LEval, n);
  LCoup := LListe[0];
  if EstUnRoque(APos, LCoup) and not AFrc then
  begin
    Assert(((LCoup and $FF00) shr 8) mod 8 = CColonneE);
    Reformule(LCoup);
  end;
  result := NomCoup(LCoup);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
