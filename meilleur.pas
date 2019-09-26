
{**
  @abstract(Fonction de recherche du meilleur coup.)
  Fonction de recherche du meilleur coup.
}

unit Meilleur;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AFRC: boolean; const ATempsDispo: integer): string;

var
  LCoupProv: string = 'a1a1';
  
implementation

uses
  SysUtils, Deplacement, Coups, Roque, Damier, Tables, Journal, Histoire, Tri, Polices;

const
  CInfini = 99999;
  CValeurPiece: array[PionBlanc..Dame] of integer = (100, 100, 500, 320, 330, 900);
  
var
  LLimiteTemps: cardinal;

function Materiel(const APos: TPosition): integer;
var
  LIdx, LClr: integer;
  LPiece: TTypePiece;
  LBlanc, LNoir, LCase: TDamier;
begin
  if (APos.Pieces[APos.Trait] and APos.Rois) = 0 then
    Exit(-1 * CInfini)
  else if (APos.Pieces[not APos.Trait] and APos.Rois) = 0 then
    Exit(CInfini);
  result := 0;
  LBlanc := APos.Pieces[FALSE];
  LNoir := APos.Pieces[TRUE];
  for LIdx := A1 to H8 do
  begin
    LCase := CCaseIdx[LIdx];
    if EstAllumee(LBlanc, LCase) then
      LClr := 1
    else if EstAllumee(LNoir, LCase) then
      LClr := -1
    else
      Continue;
    LPiece := TypePieceIdx(APos, LIdx);
    if LPiece <> Roi then
      Inc(result, CValeurPiece[LPiece] * LClr);
  end;
  if APos.Trait then
    result := -1 * result;
end;

function EstLegal(const APos: TPosition; const ACoup: integer): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  if FRejoue(LPos, NomCoup(ACoup)) then
  begin
    result := TRUE;
    LPos.Trait := not LPos.Trait;
    if FEchec(LPos) then
      Exit(FALSE);
  end else
    result := FALSE;
end;

function MiniMax(const APos: TPosition; const ACoup: integer; out AEchecProchain: boolean): integer;
var
  LLst1, LLst2: array[0..199] of integer;
  LPos1, LPos2, LPos3: TPosition;
  LIdx1, LIdx2, LNbr1, LNbr2, LRes, LMax: integer;
begin
  AEchecProchain := FALSE;
  LPos1 := APos;
  result := Low(integer);
  if FRejoue(LPos1, NomCoup(ACoup)) then
  begin
    FCoups(LPos1, LLst1, LNbr1);
    result := High(integer);
    for LIdx1 := 0 to Pred(LNbr1) do
    begin
      LPos2 := LPos1;
      if FRejoue(LPos2, NomCoup(LLst1[LIdx1])) then
      begin
        if FEchec(LPos2) then
          AEchecProchain := TRUE;
        FCoups(LPos2, LLst2, LNbr2);
        LMax := Low(integer);
        for LIdx2 := 0 to Pred(LNbr2) do
        begin
          LPos3 := LPos2;
          if FRejoue(LPos3, NomCoup(LLst2[LIdx2])) then
          begin
            LPos3.Trait := not LPos3.Trait;
            if FEchec(LPos3) then
              LRes := -1 * CInfini
            else
              LRes := Materiel(LPos3);
          end else
            Continue;
          if LRes > LMax then
          begin
            LMax := LRes;
           if LMax = CInfini then
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

function Inverse(const ACoup: string): string;
begin
  result := Concat(Copy(ACoup, 3, 2), Copy(ACoup, 1, 2));
end;

function EvaluationStatique(const APos: TPosition; const ACoup: integer): integer;
var
  LPos: TPosition;
  LBonusRoque: integer;
  LMalusRepetition, LMalusAnnulation: integer;
  LBonusPiece: integer = -2;
  LTypePiece: TTypePieceLarge;
  LDep, LArr: integer;
  LTP: TTypePiece;
  LTC: TTypeCoup;
  LBonusEnPassant, LBonusCapture: integer;
  LBonusMenaceRoi: integer;
  LBonusEchec: integer;
  LProtections: integer;
  LMalusPiece: integer;
begin
  DecodeCoup(ACoup, LDep, LArr, LTP, LTC);
  
  LPos := APos;
  LBonusRoque := Ord(EstUnRoque(LPos, ACoup)); if LBonusRoque = 1 then Assert(LTC = tcRoque);
  LMalusRepetition := Ord(NomCoup(ACoup) = AvantDernier);
  LMalusAnnulation := Ord(NomCoup(ACoup) = Inverse(Dernier));
  LTypePiece := TypePieceIdx(LPos, Depart(ACoup)); Assert(LTypePiece = LTP);
  result := Low(integer);
  if not FRejoue(LPos, NomCoup(ACoup)) then
    Exit;
  LBonusEchec := Ord(FEchec(LPos));
  LPos.Trait := not LPos.Trait;
  LBonusPiece := 0;
  LBonusEnPassant := Ord(LTC = tcEnPassant);
  LBonusCapture := Ord(LTC = tcCapture);
  LBonusMenaceRoi := Ord((CCibles[LTP, LArr] and LPos.CaseRoi[not LPos.Trait]) <> 0);
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
   
  Journal.Ajoute(
    Format(
      '<tr>' +
      '<td style="text-align: left;">%s</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '<td>%d</td>' +
      '</tr>',
      [
        NomCoup(ACoup),
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

function MeilleurCoup(const APos: TPosition; const AFRC: boolean; const ATempsDispo: integer): string;
const
  CFmtStr = '<p style="font-family:chess mark;font-size:24px;">' + LineEnding + '%s</p>';
var
  LListe, LEval: array[0..99] of integer;
  n, i, LCoup: integer;
  LEchecProchain: boolean;
begin
  result := '0000';
  LLimiteTemps := GetTickCount64 + ATempsDispo;
  Journal.Ajoute(Format('<p>%s</p>', [DateTimeToStr(Now)]), TRUE);
  Journal.Ajoute(Format(CFmtStr, [Polices.PositionToHtml(Polices.EPDToPosition(DecodePosition(APos)), AMChars)]), TRUE);
  FCoups(APos, LListe, n);
  FRoque(APos, LListe, n);
  
  for i := 0 to Pred(n) do
  begin
    LEval[i] := Ord(EstLegal(
      APos,
      LListe[i]
    ));
  end;
  Trie(LListe, LEval, n);
  LCoupProv := NomCoup(LListe[0]);
  Journal.AjouteTable(LListe, LEval, n, 'I.');
  
  n := CompteMeilleurs(LEval, n);
{$IFDEF RANDOM_MOVER}
  Exit(NomCoup(LListe[Random(n)]));
{$ENDIF}
  for i := 0 to Pred(n) do
  begin
    LEval[i] := MiniMax(
      APos,
      LListe[i],
      LEchecProchain
    );
    Dec(LEval[i], Ord(LEchecProchain));
  end;
  Trie(LListe, LEval, n);
  LCoupProv := NomCoup(LListe[0]);
  Journal.AjouteTable(LListe, LEval, n, 'II.');
  
  n := CompteMeilleurs(LEval, n);
  Journal.Ajoute(
    '<table><caption>III.</caption><tr>' +
    '<th>Coup</th>' +
    '<th>B cap</th>' +
    '<th>B men</th>' +
    '<th>B roq</th>' +
    '<th>B enp</th>' +
    '<th>B typ</th>' +
    '<th>M rép</th>' +
    '<th>M ann</th>' +
    '<th>B éch</th>' +
    '<th>B pro</th>' +
    '<th>M pie</th>' +
    '<th>Total</th>' +
    '</tr>',
    TRUE
  );
  for i := 0 to Pred(n) do
    LEval[i] := EvaluationStatique(APos, LListe[i]);
  Journal.Ajoute('</table>', TRUE);
  
  Trie(LListe, LEval, n);
  LCoup := LListe[0];
  if EstUnRoque(APos, LCoup) and not AFRC then
  begin
    Assert(((LCoup and $FF00) shr 8) mod 8 = CColonneE);
    Reformule(LCoup);
  end;
  result := NomCoup(LCoup);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
