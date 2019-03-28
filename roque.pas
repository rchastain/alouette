
unit Roque;

interface

uses
  Echecs, Journal;

procedure GenereRoque(const APos: TPosition; var AListe: array of integer; var ACompte: integer);

implementation

uses
  SysUtils, Damier, Tables, Coups;
  
procedure GenereRoque(const APos: TPosition; var AListe: array of integer; var ACompte: integer);
procedure Accepte(const i, j: integer; const ACondition: boolean = TRUE);
begin
  if ACondition then
  begin
    Inc(ACompte);
    Assert(ACompte <= Length(AListe));
    AListe[Pred(ACompte)] := EncodeCoup(i, j);
  end;
end;
var
  { Pièces. }
  actives, toutes,
  { Cases menacées par l'adversaire. }
  casesMenacees: TDamier;
  LLigneRoque,
  LCDR, { Colonne départ roi. }
  LCDT: integer; { Colonne départ tour. }
  LPos: TPosition;
  caseRoi: TDamier;
procedure Recherche(const ACAR, ACAT: integer); { Colonne arrivée roi, colonne arrivée tour. }
var
  i, j, k, l: integer;
  b, c, d: boolean;
begin
  i := FIndex(LCDR, LLigneRoque); k := FIndex(ACAR, LLigneRoque);
  j := FIndex(LCDT, LLigneRoque); l := FIndex(ACAT, LLigneRoque);
  TJournal.Ajoute(Format('Vérifications pour roi %s tour %s...', [NomCoup(i, k), NomCoup(j, l)]));
  if Allumee(actives and APos.Tours, CCase[j]) then
    TJournal.Ajoute('Position tour vérifiée (condition 1/3).')
  else exit;
  
  b := (CChemin[i, k] and toutes) = (CChemin[i, k] and APos.Tours and actives);
  c := (CompteCases(CChemin[i, k] and APos.Tours and actives) <= 1);
  d := (CChemin[j, l] and toutes) = (CChemin[i, k] and APos.Rois and actives);
  if b and c and d then
    TJournal.Ajoute('Liberté du passage vérifiée (condition 2/3).')
  else exit;
  
  if (casesMenacees and ((caseRoi or CChemin[i, k] or CChemin[l, l])) = CDamierVide) then
    TJournal.Ajoute('Absence d''empêchement vérifiée (condition 3/3). Roque accepté.')
  else exit;
  
  Accepte(i, j);
end;
const
  G = 6;
  F = 5;
  C = 2;
  D = 3;
begin
  with APos do
  begin
    if Trait then
    begin
      actives := Noires;
      LLigneRoque := 7;
      TJournal.Ajoute('Recherche roque noir...');
    end else
    begin
      actives := Blanches;
      LLigneRoque := 0;
      TJournal.Ajoute('Recherche roque blanc...');
    end;
    toutes := Blanches or Noires;
  end;
  LPos := APos;
  LPos.Trait := not LPos.Trait;
  casesMenacees := GenereCoups(LPos);
  caseRoi := APos.PositionRoi[APos.Trait];
  LCDR := Colonne(caseRoi);
  LCDT := APos.Roque[APos.Trait].XTourRoi;
  if (LCDT >= 0) and (LCDT <= 7) then
  begin
    TJournal.Ajoute('Côté Roi...');
    Recherche(G, F);
  end;
  LCDT := APos.Roque[APos.Trait].XTourDame;
  if (LCDT >= 0) and (LCDT <= 7) then
  begin
    TJournal.Ajoute('Côté Dame...');
    Recherche(C, D);
  end;
end;

end.
