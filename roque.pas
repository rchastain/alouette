
{**
  @abstract(Roque.)
  Génération du roque.
}

unit Roque;

interface

uses
  Echecs, Journal;

procedure FRoque(const APos: TPosition; var AListe: array of integer; var ACompte: integer);

implementation

uses
  SysUtils, Damier, Tables, Coups;
  
procedure FRoque(const APos: TPosition; var AListe: array of integer; var ACompte: integer);
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
  toutes,
  { Cases menacées par l'adversaire. }
  menacees: TDamier;
  LLigRoq,
  LColDepRoi, { Colonne départ roi. }
  LColDepTour: integer; { Colonne départ tour. }
  LPos: TPosition;
  caseRoi: TDamier;
procedure Recherche(const ACAR, ACAT: integer); { Colonne arrivée roi, colonne arrivée tour. }
var
  i, j, k, l: integer;
  b, c, d: boolean;
  parcours: TDamier; { Le chemin, y compris la case d'arrivée. }
  autorisees: TDamier; { Les pièces autorisées sur le parcours. }
begin
  i := FIndex(LColDepRoi, LLigRoq); k := FIndex(ACAR, LLigRoq);
  j := FIndex(LColDepTour, LLigRoq); l := FIndex(ACAT, LLigRoq);
  //TJournal.Ajoute(Format('Vérifications pour roi %s tour %s...', [NomCoup(i, k), NomCoup(j, l)]));
  if EstAllumee({actives}APos.PiecesCouleur[APos.Trait] and APos.Tours, CCaseIdx[j]) then
    //TJournal.Ajoute('Position tour vérifiée (condition 1/3).')
  else exit;
  
  parcours := CChemin[i, k] or CCaseIdx[k];
  autorisees := APos.Tours and {actives}APos.PiecesCouleur[APos.Trait];
  b := (parcours and toutes) = (parcours and autorisees);
  c := (CompteCasesAllumees(parcours and autorisees) <= 1);
  parcours := CChemin[j, l] or CCaseIdx[l];
  autorisees := APos.Rois and {actives}APos.PiecesCouleur[APos.Trait];
  d := (parcours and toutes) = (parcours and autorisees);
  if b and c and d then
    //TJournal.Ajoute('Liberté de passage vérifiée (condition 2/3).')
  else exit;
  
  if (menacees and ((CCaseIdx[i] or CChemin[i, k] or CCaseIdx[k])) = 0) then
    //TJournal.Ajoute('Absence d''empêchement vérifiée (condition 3/3). Roque accepté.')
  else exit;
  
  Accepte(i, j);
end;

begin
  with APos do
  begin
    if Trait then
      LLigRoq := CLig8
    else
      LLigRoq := CLig1;
    toutes := PiecesCouleur[FALSE] or PiecesCouleur[TRUE];
  end;
  LPos := APos;
  LPos.Trait := not LPos.Trait;
  menacees := FCoups(LPos) or FCoupsPotentielsPion(LPos);
  caseRoi := APos.PositionRoi[APos.Trait];
  LColDepRoi := Colonne(caseRoi);
  LColDepTour := APos.Roque[APos.Trait].XTourRoi;
  if (LColDepTour >= 0) and (LColDepTour <= 7) then
    Recherche(CColG, CColF);
  LColDepTour := APos.Roque[APos.Trait].XTourDame;
  if (LColDepTour >= 0) and (LColDepTour <= 7) then
    Recherche(CColC, CColD);
end;

end.
