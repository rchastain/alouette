
{**
  @abstract(Fonction de recherche du meilleur coup.)
  Fonction de recherche du meilleur coup.
}

unit MeilleurA;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AFRC: boolean; const ATempsDispo: integer): string;

implementation

uses
  SysUtils, Deplacement, Coups, Roque, Damier, Tables, Journal, Histoire, Tri, Polices;

const
  CInfini = 99999;
  CValeurPiece: array[PionBlanc..Dame] of integer = (
    100, 100, 500, 320, 330, 900
  );
  
var
  LLimiteTemps: cardinal;

function Materiel(const APos: TPosition): integer;
var
  LIdx, LClr: integer;
  LPiece: TTypePiece;
  LBlanches, LNoires, LCase: TDamier;
begin
  if (APos.PiecesCouleur[APos.Trait] and APos.Rois) = 0 then
    Exit(-1 * CInfini)
  else if (APos.PiecesCouleur[not APos.Trait] and APos.Rois) = 0 then
    Exit(CInfini);
  result := 0;
  LBlanches := APos.PiecesCouleur[FALSE];
  LNoires := APos.PiecesCouleur[TRUE];
  for LIdx := A1 to H8 do
  begin
    LCase := CCaseIdx[LIdx];
    if EstAllumee(LBlanches, LCase) then
      LClr := 1
    else if EstAllumee(LNoires, LCase) then
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

function OpponentMax(const APos: TPosition; const ACoup: integer): integer;
var
  LLst1, LLst2: array[0..99] of integer;
  LPos1, LPos2, LPos3: TPosition;
  i, j, n, o, v, w: integer;
begin
  LPos1 := APos;
  result := Low(integer);
  if FRejoue(LPos1, NomCoup(ACoup)) then
  begin
    FCoups(LPos1, LLst1, n);
    for i := 0 to Pred(n) do
    begin
      LPos2 := LPos1;
      if FRejoue(LPos2, NomCoup(LLst1[i])) then
      begin
        LPos2.Trait := not LPos2.Trait;
        v := Materiel(LPos2);
          (* Le joueur n'a plus de roi. *)
        if v = CInfini then
          Exit(CInfini)
        else
          if v > result then
            result := v;
      end;
    end;
  end;
end;

function Inverse(const ACoup: string): string;
begin
  result := Concat(Copy(ACoup, 3, 2), Copy(ACoup, 1, 2));
end;

function BonusPositionPion(const APos: TPosition; const AIdx: integer; const ACouleurPion: TTypePiece): integer;
var
  amis: TDamier;
begin
  result := 0;
  amis := APos.PiecesCouleur[APos.Trait] and APos.Pions;
  case ACouleurPion of
    PionBlanc: if (amis and CCibles[PionNoir, AIdx]) <> 0 then result := 1;
    PionNoir: if (amis and CCibles[PionBlanc, AIdx]) <> 0 then result := 1;
  end;
  if result = 1 then
    Inc(result, 4 - Abs(AIdx mod 8 - 4));
end;

function DeuxiemeEvaluation(const APos: TPosition; const ACoup: integer): integer;
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
begin
  DecodeCoup(ACoup, LDep, LArr, LTP, LTC);
  
  LPos := APos;
  LBonusRoque := 50 * Ord(EstUnRoque(LPos, ACoup));
  if LBonusRoque = 50 then Assert(LTC = tcRoque);
  LMalusRepetition := -1 * Ord(NomCoup(ACoup) = AvantDernier);
  LMalusAnnulation := -1 * Ord(NomCoup(ACoup) = Inverse(Dernier));
  LTypePiece := TypePieceIdx(LPos, Depart(ACoup));
  Assert(LTypePiece = LTP);
  result := Low(integer);
  if not FRejoue(LPos, NomCoup(ACoup)) then
    Exit; 
  
  LPos.Trait := not LPos.Trait;
  case LTypePiece of
    PionBlanc, PionNoir:
      LBonusPiece := BonusPositionPion(LPos, Arrivee(ACoup), LTypePiece) + 2;
    Cavalier, Fou:
      LBonusPiece := 1;
    Tour, Dame:
      LBonusPiece := 0;
    Roi:
      LBonusPiece := -1;
  end;
  
  LBonusEnPassant := 100 * Ord(LTC = tcEnPassant);
  LBonusCapture := 50 * Ord(LTC = tcCapture);
  LBonusMenaceRoi := 50 * Ord((CCibles[LTP, LArr] and APos.CaseRoi[not LPos.Trait]) <> 0);
  
  result :=
    0
    + LBonusCapture
    + LBonusMenaceRoi
    + LBonusRoque
    + LBonusEnPassant
    + LBonusPiece
    + LMalusRepetition
    + LMalusAnnulation;
  
  TJournal.Ajoute(
    Format(
      '<tr><td style="text-align: left;">%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
      [
        NomCoup(ACoup),
        LBonusCapture,
        LBonusMenaceRoi,
        LBonusRoque,
        LBonusEnPassant,
        LBonusPiece,
        LMalusRepetition,
        LMalusAnnulation,
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
  //CFmtStr = '<p style="font-family:chess alfonso-x;color:midnightblue;font-size:32px;">%s</p>';
  CFmtStr = '<p style="font-family:chess mark;font-size:24px;">' + LineEnding + '%s</p>';
var
  LListe, LEval: array[0..99] of integer;
  n, i, coup: integer;
begin
  result := '0000';
  LLimiteTemps := GetTickCount64 + ATempsDispo;
  TJournal.Ajoute(Format('<p>%s</p>', [DateTimeToStr(Now)]), TRUE);
  TJournal.Ajoute(Format(CFmtStr, [polices.PositionToHtml(polices.EPDToPosition(DecodePosition(APos)), AMChars)]), TRUE);
  
  FCoups(APos, LListe, n);
  FRoque(APos, LListe, n);
  
  for i := 0 to Pred(n) do
    LEval[i] := -1 * OpponentMax(
      APos,
      LListe[i]
    );
  Trie(LListe, LEval, n);
  TJournal.AjouteTable(LListe, LEval, n, 'I.');

  n := CompteMeilleurs(LEval, n);
  TJournal.Ajoute('<table><caption>II.</caption><tr><th>Coup</th><th>B cap</th><th>B men</th><th>B roq</th><th>B enp</th><th>B typ</th><th>M rép</th><th>M ann</th><th>Total</th></tr>', TRUE);
  for i := 0 to Pred(n) do
    LEval[i] := DeuxiemeEvaluation(APos, LListe[i]);
  TJournal.Ajoute('</table>', TRUE);
  
  Trie(LListe, LEval, n);
  coup := LListe[0];
  
  if EstUnRoque(APos, coup) and not AFRC then
  begin
    Assert(((coup and $FF00) shr 8) mod 8 = CColonneE);
    Reformule(coup);
  end;
  
  result := NomCoup(coup);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
