
{**
  @abstract(Fonction de recherche du meilleur coup.)
  Fonction de recherche du meilleur coup.
}

unit Meilleur;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean; const ATimeAvailable: cardinal = 1000): string;

implementation

uses
  SysUtils, Deplacement, Coups, Roque, Damier, Tables, Journal, Histoire, Tri;

const
  CMaximumMateriel =  99999;
  CMinimumMateriel = -99999;
  
var
  LStartTime, LTimeAvailable: cardinal;
  
function Materiel(const APos: TPosition): integer;
var
  LIdx, LCoul: integer;
  LPieces: TDamier;
begin
  LPieces := APos.PiecesCouleur[APos.Trait] and APos.Rois;
  if LPieces = 0 then Exit(CMinimumMateriel);
  
  LPieces := APos.PiecesCouleur[not APos.Trait] and APos.Rois;
  if LPieces = 0 then Exit(CMaximumMateriel);
  
  result := 0;
  for LIdx := A1 to H8 do
  begin
    if EstAllumeeIdx(APos.PiecesCouleur[FALSE], LIdx) then
      LCoul := 1
    else if EstAllumeeIdx(APos.PiecesCouleur[TRUE], LIdx) then
      LCoul := -1
    else
      continue;
    if EstAllumeeIdx(APos.Pions,     LIdx) then Inc(result,   100 * LCoul) else
    if EstAllumeeIdx(APos.Cavaliers, LIdx) then Inc(result,   320 * LCoul) else
    if EstAllumeeIdx(APos.Fous,      LIdx) then Inc(result,   330 * LCoul) else
    if EstAllumeeIdx(APos.Tours,     LIdx) then Inc(result,   500 * LCoul) else
    if EstAllumeeIdx(APos.Dames,     LIdx) then Inc(result,   900 * LCoul) else
    //if EstAllumeeIdx(APos.Rois,      LIdx) then Inc(result, 20000 * LCoul);
  end;
  if APos.Trait then
    result := -1 * result;
end;

const
  CNombreRecursion = 1;
  
function PremiereEvaluation(const APos: TPosition; const ACoup: integer; const AEtape: integer = CNombreRecursion): integer;
var
  LPos1, LPos2, LPos3: TPosition;
  LListe1, LListe2: array[0..99] of integer;
  n, o, i, j, v, vmax: integer;
begin
  LPos1 := APos;
  result := Low(integer);
  
  if not FRejoue(LPos1, NomCoup(ACoup)) then
  begin
    TJournal.Ajoute(Format('Impossible de rejouer %s (ligne %s).', [NomCoup(ACoup), {$I %LINE%}]));
    exit;
  end;
  FCoups(LPos1, LListe1, n);
  result := High(integer);
  
  for i := 0 to Pred(n) do
  begin
    LPos2 := LPos1;
    if not FRejoue(LPos2, NomCoup(LListe1[i])) then
    begin
      TJournal.Ajoute(Format('Impossible de rejouer %s (ligne %s).', [NomCoup(ACoup), {$I %LINE%}]));
      continue;
    end;
    
    if Materiel(LPos2) = CMinimumMateriel then
    begin
      TJournal.Ajoute(Format('Le joueur n''a plus de roi (ligne %s).', [{$I %LINE%}]));
      exit(CMinimumMateriel - AEtape)
    end;
    
    FCoups(LPos2, LListe2, o);
    vmax := Low(integer);
    for j := 0 to Pred(o) do
    begin
      LPos3 := LPos2;
      if (AEtape = 0) or (GetTickCount64 - LStartTime > 9 * LTimeAvailable div 10) then
      begin
        if not FRejoue(LPos3, NomCoup(LListe2[j])) then
        begin
          TJournal.Ajoute(Format('Impossible de rejouer %s (ligne %s).', [NomCoup(ACoup), {$I %LINE%}]));
          continue;
        end;
        LPos3.Trait := not LPos3.Trait;
        v := Materiel(LPos3);
      end else
      begin
        v := PremiereEvaluation(LPos3, LListe2[j], Pred(AEtape));
      end;
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
(*
  function Estime(const APieces: TDamier): integer;
  var
    LPieces: TDamier;
  begin
    result := 0;
    LPieces := APieces and LPos.Rois;      if LPieces <> 0 then Inc(result, 20000);
    LPieces := APieces and LPos.Dames;     if LPieces <> 0 then Inc(result,   900 * CompteCasesAllumees(LPieces));
    LPieces := APieces and LPos.Tours;     if LPieces <> 0 then Inc(result,   500 * CompteCasesAllumees(LPieces));
    LPieces := APieces and LPos.Fous;      if LPieces <> 0 then Inc(result,   330 * CompteCasesAllumees(LPieces));
    LPieces := APieces and LPos.Cavaliers; if LPieces <> 0 then Inc(result,   320 * CompteCasesAllumees(LPieces));
    LPieces := APieces and LPos.Pions;     if LPieces <> 0 then Inc(result,   100 * CompteCasesAllumees(LPieces));
  end;
*)
var
  //LMenaces: TDamier;
  LBonusRoque{, LMalusCapturesPotentielles, LBonusNombreCoups, LBonusTables}: integer;
  //LBonusCapturesPotentielles: integer;
  //LBonusProtection: integer;
  LMalusRepetition, LMalusAnnulation: integer;
begin
  LPos := APos;
  LBonusRoque := 100 * Ord(EstUnRoque(LPos, ACoup));
  result := Low(integer);
  if not FRejoue(LPos, NomCoup(ACoup)) then
    exit; 

  //LMenaces := FCoups(LPos);
  //LMalusCapturesPotentielles := Estime(LMenaces and LPos.PiecesCouleur[not LPos.Trait]);
  LPos.Trait := not LPos.Trait;
  //LBonusNombreCoups := FNombreCoups(LPos) div 2;
  //LBonusTables := ResultatTables(LPos) div 5;
  //LBonusCapturesPotentielles := Estime(FCoups(LPos) and LPos.PiecesCouleur[LPos.Trait]);
  //LBonusProtection := FProtections(LPos);
  LMalusRepetition := Ord(NomCoup(ACoup) = AvantDernier);
  LMalusAnnulation := Ord(NomCoup(ACoup) = Inverse(Dernier));
  
  result :=
    0
    //+ LBonusTables
    + LBonusRoque
    //+ LBonusNombreCoups
    //+ LBonusCapturesPotentielles
    //+ LBonusProtection
    //- LMalusCapturesPotentielles
    - LMalusRepetition
    - LMalusAnnulation;
  
  TJournal.Ajoute(
    Format(
      '<tr><td style="text-align: left;">%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
      [
        NomCoup(ACoup),
        result,
        //LBonusTables,
        LBonusRoque,
        //LBonusNombreCoups,
        //LBonusCapturesPotentielles,
        //LBonusProtection,
        //LMalusCapturesPotentielles,
        LMalusRepetition,
        LMalusAnnulation
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

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean; const ATimeAvailable: cardinal): string;
var
  LListe, LEval: array[0..99] of integer;
  n, i, coup: integer;
begin
  result := '0000';
  LStartTime := GetTickCount64;
  LTimeAvailable := ATimeAvailable;
  TJournal.Ajoute(Format('<p>%s</p>', [DateTimeToStr(Now)]), TRUE);
  FCoups(APos, LListe, n);
  FRoque(APos, LListe, n);
  for i := 0 to Pred(n) do
    LEval[i] := PremiereEvaluation(
      APos,
      LListe[i]
    );
  Trie(LListe, LEval, n);
  TJournal.AjouteTable(LListe, LEval, n, 'Première évaluation');
  
  n := CompteMeilleurs(LEval, n);
  
  TJournal.Ajoute('<table><caption>Détail deuxième évaluation</caption><tr><th>Coup</th><th>Total</th><th>B roq</th><th>M rép</th><th>M ann</th></tr>', TRUE);
  for i := 0 to Pred(n) do
    LEval[i] := DeuxiemeEvaluation(APos, LListe[i]);
  TJournal.Ajoute('</table>', TRUE);
  Trie(LListe, LEval, n);
  TJournal.AjouteTable(LListe, LEval, n, 'Deuxième évaluation');
  
  coup := LListe[0];
  if EstUnRoque(APos, coup) and not AEchecs960 then
  begin
    Assert((coup div 100) mod 8 = CColonneE);
    Reformule(coup);
  end;
  
  result := NomCoup(coup);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
