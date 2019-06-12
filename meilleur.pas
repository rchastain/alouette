
{**
  @abstract(Fonction de recherche du meilleur coup.)
  Fonction de recherche du meilleur coup.
}

unit Meilleur;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AFRC: boolean; const ATempsDispo: cardinal = 60 * 1000): string;

implementation

uses
  SysUtils, Deplacement, Coups, Roque, Damier, Tables, Journal, Histoire, Tri, Fonts;

const
  CInfini = 99999;
  
var
  LStartTime, LTimeAvail: cardinal;
  
function Materiel(const APos: TPosition): integer;
var
  LIdx, LClr: integer;
  LPcs: TDamier;
begin
  LPcs := APos.PiecesCouleur[APos.Trait] and APos.Rois;
  if LPcs = 0 then exit(-1 * CInfini);
  LPcs := APos.PiecesCouleur[not APos.Trait] and APos.Rois;
  if LPcs = 0 then exit(CInfini);
  result := 0;
  for LIdx := A1 to H8 do
  begin
    if EstAllumeeIdx(APos.PiecesCouleur[FALSE], LIdx) then
      LClr := 1
    else if EstAllumeeIdx(APos.PiecesCouleur[TRUE], LIdx) then
      LClr := -1
    else
      continue;
    if EstAllumeeIdx(APos.Pions,     LIdx) then Inc(result,   100 * LClr) else
    if EstAllumeeIdx(APos.Cavaliers, LIdx) then Inc(result,   320 * LClr) else
    if EstAllumeeIdx(APos.Fous,      LIdx) then Inc(result,   330 * LClr) else
    if EstAllumeeIdx(APos.Tours,     LIdx) then Inc(result,   500 * LClr) else
    if EstAllumeeIdx(APos.Dames,     LIdx) then Inc(result,   900 * LClr) else
  end;
  if APos.Trait then
    result := -1 * result;
end;

const
  CNombreRecursion = 1;
  
function MiniMax(const APos: TPosition; const ACoup: integer; const AEtape: integer = CNombreRecursion): integer;
var
  LLst1, LLst2: array[0..99] of integer;
  LPos1, LPos2, LPos3: TPosition;
  i, j, n, o, v, w: integer;
  LSkip: boolean;
begin
  LPos1 := APos;
  result := Low(integer);
  if not FRejoue(LPos1, NomCoup(ACoup)) then
  begin
    TJournal.Ajoute(Format('Impossible de rejouer %s (ligne %s).', [NomCoup(ACoup), {$I %LINE%}]));
    exit;
  end;
  FCoups(LPos1, LLst1, n);
  result := High(integer);
  for i := 0 to Pred(n) do
  begin
    LPos2 := LPos1;
    if not FRejoue(LPos2, NomCoup(LLst1[i])) then
    begin
      TJournal.Ajoute(Format('Impossible de rejouer %s (ligne %s).', [NomCoup(ACoup), {$I %LINE%}]));
      continue;
    end;
    if Materiel(LPos2) = -1 * CInfini then // Le joueur n'a plus de roi.
      exit(-1 * CInfini - AEtape);
    FCoups(LPos2, LLst2, o);
    w := Low(integer);
    LSkip := FALSE;
    for j := 0 to Pred(o) do
    begin
      LPos3 := LPos2;
      if (AEtape = 0) or (GetTickCount64 - LStartTime > LTimeAvail - 200) then
      begin
        if (AEtape = 0) and not EstAllumeeIdx(LPos3.PiecesCouleur[not LPos3.Trait], LLst2[j] mod 100) then // Ignore la prise en passant !
          if LSkip then
            continue
          else
            LSkip := TRUE;
        if not FRejoue(LPos3, NomCoup(LLst2[j])) then
        begin
          TJournal.Ajoute(Format('Impossible de rejouer %s (ligne %s).', [NomCoup(ACoup), {$I %LINE%}]));
          continue;
        end;
        LPos3.Trait := not LPos3.Trait;
        v := Materiel(LPos3);
      end else
      begin
        v := MiniMax(LPos3, LLst2[j], Pred(AEtape));
      end;
      if v > w then
      begin
        w := v;
       {if w = CInfini then
          break;}
      end;
    end;
    if w < result then
    begin
      result := w;
     {if result = -1 * CInfini then
        break;}
    end;
  end;
end;

function Inverse(const ACoup: string): string;
begin
  result := Concat(Copy(ACoup, 3, 2), Copy(ACoup, 1, 2));
end;

function DeuxiemeEvaluation(const APos: TPosition; const ACoup: integer): integer;
var
  LPos: TPosition;
var
  LBonusRoque: integer;
  LMalusRepetition, LMalusAnnulation: integer;
  LBonusPion: integer = -2;
  LTypePiece: TPieceEtendu;
begin
  LPos := APos;
  LBonusRoque := 100 * Ord(EstUnRoque(LPos, ACoup));
  LTypePiece := TypePiece(LPos, ACoup div 100);
  result := Low(integer);
  if not FRejoue(LPos, NomCoup(ACoup)) then
    exit; 
  LPos.Trait := not LPos.Trait;
  LMalusRepetition := Ord(NomCoup(ACoup) = AvantDernier);
  LMalusAnnulation := Ord(NomCoup(ACoup) = Inverse(Dernier));
  case LTypePiece of
    PionBlanc, PionNoir: LBonusPion := 2;
    Cavalier, Fou: LBonusPion := 1;
    Tour, Dame: LBonusPion := 0;
    Roi: LBonusPion := -1;
  end;
  
  result :=
    0
    + LBonusRoque
    + LBonusPion
    - LMalusRepetition
    - LMalusAnnulation;
  
  TJournal.Ajoute(
    Format(
      '<tr><td style="text-align: left;">%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
      [
        NomCoup(ACoup),
        LBonusRoque,
        LBonusPion,
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

function MeilleurCoup(const APos: TPosition; const AFRC: boolean; const ATempsDispo: cardinal): string;
const
  //CFmtStr = '<p style="font-family:chess alfonso-x;color:midnightblue;font-size:32px;">%s</p>';
  CFmtStr = '<p style="font-family:chess alfonso-x;font-size:24px;">%s</p>';
var
  LListe, LEval: array[0..99] of integer;
  n, i, coup: integer;
begin
  result := '0000';
  LStartTime := GetTickCount64;
  LTimeAvail := ATempsDispo;
  TJournal.Ajoute(Format('<p>%s</p>', [DateTimeToStr(Now)]), FALSE);
  TJournal.Ajoute(Format(CFmtStr, [PositionToHtml({gPosition}EPDToPosition(DecodePosition(APos)), AMChars)]), TRUE);
  FCoups(APos, LListe, n);
  FRoque(APos, LListe, n);
  for i := 0 to Pred(n) do
    LEval[i] := MiniMax(
      APos,
      LListe[i]
    );
  Trie(LListe, LEval, n);
  TJournal.AjouteTable(LListe, LEval, n, 'Première évaluation');
  
  n := CompteMeilleurs(LEval, n);
  
  TJournal.Ajoute('<table><caption>Deuxième évaluation</caption><tr><th>Coup</th><th>B roq</th><th>B typ</th><th>M rép</th><th>M ann</th><th>Total</th></tr>', TRUE);
  for i := 0 to Pred(n) do
    LEval[i] := DeuxiemeEvaluation(APos, LListe[i]);
  TJournal.Ajoute('</table>', TRUE);
  
  Trie(LListe, LEval, n);
  coup := LListe[0];
  if EstUnRoque(APos, coup) and not AFRC then
  begin
    Assert((coup div 100) mod 8 = CColonneE);
    Reformule(coup);
  end;
  
  result := NomCoup(coup);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
