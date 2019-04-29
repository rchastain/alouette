
{**
  @abstract(Fonction de recherche du meilleur coup.)
  Fonction de recherche du meilleur coup.
}

unit Meilleur;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean): string;

implementation

uses
  SysUtils, Deplacement, Coups, Roque, Damier, Tables, Journal, Histoire, TablesPieceCase, Tri;

function ResultatTables(const APos: TPosition): integer;
var
  LActives: TDamier;
  LInd, LIndInv: integer;
begin
  result := 0;
  
  with APos do
    if Trait then
      LActives := Noires
    else
      LActives := Blanches;
  
  for LInd := A1 to H8 do
    if EstAllumeeIndex(LActives, LInd) then
    begin
      if not APos.Trait then
        LIndInv := 63 - LInd
      else
        LIndInv := LInd;
      if EstAllumeeIndex(APos.Pions,     LInd) then Inc(result, CTablePionBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Cavaliers, LInd) then Inc(result, CTableCavalierBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Fous,      LInd) then Inc(result, CTableFouBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Tours,     LInd) then Inc(result, CTableTourBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Dames,     LInd) then Inc(result, CTableDameBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Rois,      LInd) then Inc(result, CTableRoiBlanc[LIndInv]);
    end;
end;

function Materiel(const APos: TPosition): integer;
var
  LInd, LCoul: integer;
begin
  result := 0;
  for LInd := A1 to H8 do
  begin
    if EstAllumeeIndex(APos.Blanches, LInd) then
      LCoul := 1
    else if EstAllumeeIndex(APos.Noires, LInd) then
      LCoul := -1
    else
      continue;
    if EstAllumeeIndex(APos.Pions,     LInd) then Inc(result,   100 * LCoul) else
    if EstAllumeeIndex(APos.Cavaliers, LInd) then Inc(result,   320 * LCoul) else
    if EstAllumeeIndex(APos.Fous,      LInd) then Inc(result,   330 * LCoul) else
    if EstAllumeeIndex(APos.Tours,     LInd) then Inc(result,   500 * LCoul) else
    if EstAllumeeIndex(APos.Dames,     LInd) then Inc(result,   900 * LCoul) else
    if EstAllumeeIndex(APos.Rois,      LInd) then Inc(result, 20000 * LCoul);
  end;
  if APos.Trait then
    result := -1 * result;
end;

function PremiereEvaluation(const APos: TPosition; const ACoup: integer): integer;
var
  LPos1, LPos2, LPos3: TPosition;
  LListe1, LListe2: array[0..99] of integer;
  n, o, i, j, v, vmax: integer;
begin
  LPos1 := APos;
  result := Low(integer);
  if not Rejoue_(LPos1, NomCoup(ACoup)) then
    exit;
  ChercheCoups(LPos1, LListe1, n);
  result := High(integer);
  
  for i := 0 to Pred(n) do
  begin
    LPos2 := LPos1;
    if not Rejoue_(LPos2, NomCoup(LListe1[i])) then
    begin
      continue;
      WriteLn('Impossible de rejouer le coup ', NomCoup(LListe1[i]), '.');
    end;
    
    if Materiel(LPos2) < 8 * 900 + 2 * 500 + 2 * 330 + 2 * 320 - 20000 then
      exit(Low(integer));
    
    ChercheCoups(LPos2, LListe2, o);
    vmax := Low(integer);
    for j := 0 to Pred(o) do
    begin
      LPos3 := LPos2;
      if not Rejoue_(LPos3, NomCoup(LListe2[j])) then
      begin
        continue;
        WriteLn('Impossible de rejouer le coup ', NomCoup(LListe2[i]), '.');
      end;
      LPos3.Trait := not LPos3.Trait;
      v := Materiel(LPos3);
      if v > vmax then
        vmax := v;
    end;
    if vmax < result then
      result := vmax;
  end;
end;

function Inverse(const ACoup: string): string;
begin
  result := Concat(Copy(ACoup, 3, 2), Copy(ACoup, 1, 2));
end;

function DeuxiemeEvaluation(const APos: TPosition; const ACoup: integer): integer;
var
  LPos: TPosition;

  function Estime(const APieces: TDamier): integer;
  var
    LPieces: TDamier;
  begin
    result := 0;
    LPieces := APieces and LPos.Rois;      if LPieces <> 0 then Inc(result, 20000);
    LPieces := APieces and LPos.Dames;     if LPieces <> 0 then Inc(result,   900 * CompteCases(LPieces));
    LPieces := APieces and LPos.Tours;     if LPieces <> 0 then Inc(result,   500 * CompteCases(LPieces));
    LPieces := APieces and LPos.Fous;      if LPieces <> 0 then Inc(result,   330 * CompteCases(LPieces));
    LPieces := APieces and LPos.Cavaliers; if LPieces <> 0 then Inc(result,   320 * CompteCases(LPieces));
    LPieces := APieces and LPos.Pions;     if LPieces <> 0 then Inc(result,   100 * CompteCases(LPieces));
  end;

var
  LActives, LPassives, LMenaces: TDamier;
  LBonusRoque, LMalusCapturesPotentiellesAdv, LBonusNombreCoups, LBalanceMateriel, LBonusTables: integer;
  LBonusCapturesPotentielles: integer;
  LBonusProtection: integer;
  LMalusRepetition, LMalusAnnulation: integer;
begin
  LPos := APos;
  LBonusRoque := 100 * Ord(EstUnRoque(LPos, ACoup));
  result := Low(integer);
  if not Rejoue_(LPos, NomCoup(ACoup)) then
    exit; 
  
  with LPos do
    if Trait then begin
      LActives := Noires;
      LPassives := Blanches;
    end else begin
      LActives := Blanches;
      LPassives := Noires;
    end;

  LMenaces := ChercheCoups(LPos);
  LMalusCapturesPotentiellesAdv := Estime(LMenaces and LPassives);
  LPos.Trait := not LPos.Trait;
  LBonusNombreCoups := ChercheNombre(LPos);
  LBonusTables := ResultatTables(LPos);
  LBonusCapturesPotentielles := Estime(ChercheCoups(LPos) and LActives);
  LBonusProtection := ChercheProtections(LPos);
  LMalusRepetition := Ord(NomCoup(ACoup) = AvantDernier);
  LMalusAnnulation := Ord(NomCoup(ACoup) = Inverse(Dernier));
  
  result :=
    0
    + LBonusTables
    + LBonusRoque
    + LBonusNombreCoups
    + LBonusCapturesPotentielles
    + LBonusProtection
    - LMalusCapturesPotentiellesAdv
    - LMalusRepetition
    - LMalusAnnulation;
  
  TJournal.Ajoute(
    Format(
      '%8s%8d%8d%8d%8d%8d%8d%8d%8d%8d',
      [
        NomCoup(ACoup),
        result,
        LBonusTables,
        LBonusRoque,   
        LBonusNombreCoups,
        LBonusCapturesPotentielles,
        LBonusProtection,
        LMalusCapturesPotentiellesAdv,
        LMalusRepetition,
        LMalusAnnulation
      ]
    )
  );
end;

function LigneInfo1(const ACoups: array of integer; const n: integer; const ASeparateur: string): string;
var
  i: integer;
  LSeparateur: array[boolean] of string = ('', '');
begin
  result := '';
  LSeparateur[TRUE] := ASeparateur;
  for i := 0 to Pred(n) do
    result := Format('%s%6s%s', [
      result,
      NomCoup(ACoups[i]),
      LSeparateur[i < Pred(n)]
    ]);
end;

function LigneInfo2(const ANotes: array of integer; const n: integer; const ASeparateur: string): string;
var
  i: integer;
  LSeparateur: array[boolean] of string = ('', '');
begin
  result := '';
  LSeparateur[TRUE] := ASeparateur;
  for i := 0 to Pred(n) do
    result := Format('%s%6d%s', [
      result,
      ANotes[i],
      LSeparateur[i < Pred(n)]
    ]);
end;

function CompteMeilleurs(const ANotes: array of integer): integer;
begin
  result := 1;
  while ANotes[result] = ANotes[0] do
    Inc(result);
end;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean): string;
var
  LListe, LEval: array[0..99] of integer;
  n, i, coup: integer;
begin
  result := '0000';
  
  ChercheCoups(APos, LListe, n);
  ChercheRoque(APos, LListe, n);
  
  for i := 0 to Pred(n) do
    LEval[i] := PremiereEvaluation(APos, LListe[i]);
  Trie(LListe, LEval, n);
  TJournal.Ajoute(LigneInfo1(LListe, n, ' '));
  TJournal.Ajoute(LigneInfo2(LEval, n, ' '));
  TJournal.Ajoute('    coup   total  tables   roque   coups    capt  protec  advers   repet   annul');
  n := CompteMeilleurs(LEval);
  for i := 0 to Pred(n) do
    LEval[i] := DeuxiemeEvaluation(APos, LListe[i]);
  Trie(LListe, LEval, n);
  TJournal.Ajoute(LigneInfo1(LListe, n, ' '));
  TJournal.Ajoute(LigneInfo2(LEval, n, ' '));
  coup := LListe[0];
  if EstUnRoque(APos, coup) and not AEchecs960 then
  begin
    Assert((coup div 100) mod 8 = CColE);
    Reformule(coup);
  end;
  result := NomCoup(coup);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
