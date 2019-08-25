
{**
  @abstract(Roque.)
  Génération du roque.
}

unit Roque;

interface

uses
  Echecs, Journal;

procedure FRoque(const APos: TPosition; var ALst: array of integer; var ACpt: integer);

implementation

uses
  SysUtils, Damier, Tables, Coups;

procedure FRoque(const APos: TPosition; var ALst: array of integer; var ACpt: integer);

  procedure Accepte(const i, j: integer; const ACondit: boolean = TRUE);
  begin
    if ACondit then
    begin
      Inc(ACpt);
      Assert(ACpt <= Length(ALst));
      ALst[Pred(ACpt)] := EncodeCoup(i, j, Roi, tcRoque);
    end;
  end;

var
  LTout,                                          { Toutes les pièces. }
  LMen: TDamier;                                  { Cases menacées par l'adversaire. }
  LLigRoq,
  LColDepRoi,                                     { Colonne départ roi. }
  LColDepTour: integer;                           { Colonne départ tour. }
  LPos: TPosition;

  procedure Recherche(const ACAR, ACAT: integer); { Colonne arrivée roi, colonne arrivée tour. }
  var
    i, j, k, l: integer;
    b, c, d: boolean;
    LPar: TDamier;                                { Le chemin, y compris la case d'arrivée. }
    LAut: TDamier;                                { Les pièces autorisées sur le parcours. }
  begin
    i := FIndex(LColDepRoi,  LLigRoq); k := FIndex(ACAR, LLigRoq);
    j := FIndex(LColDepTour, LLigRoq); l := FIndex(ACAT, LLigRoq);
    Journal.Ajoute(Format('Vérifications pour roi %s tour %s...', [NomCoup(i, k), NomCoup(j, l)]));
    if EstAllumee(APos.Pieces[APos.Trait] and APos.Tours, CCaseIdx[j]) then
      Journal.Ajoute('Position tour vérifiée (condition 1/3).')
    else exit;
    LPar := CChemin[i, k] or CCaseIdx[k];
    LAut := APos.Tours and APos.Pieces[APos.Trait];
    b := (LPar and LTout) = (LPar and LAut);
    c := (CompteCasesAllumees(LPar and LAut) <= 1);
    LPar := CChemin[j, l] or CCaseIdx[l];
    LAut := APos.Rois and APos.Pieces[APos.Trait];
    d := (LPar and LTout) = (LPar and LAut);
    if b and c and d then
      Journal.Ajoute('Liberté de passage vérifiée (condition 2/3).')
    else exit;
    if (LMen and ((CCaseIdx[i] or CChemin[i, k] or CCaseIdx[k])) = 0) then
      Journal.Ajoute('Absence d''empêchement vérifiée (condition 3/3). Roque accepté.')
    else exit;
    Accepte(i, j);
  end;

begin
  with APos do
  begin
    if Trait then
      LLigRoq := CLigne8
    else
      LLigRoq := CLigne1;
    LTout := Pieces[FALSE] or Pieces[TRUE];
  end;
  LPos := APos;
  LPos.Trait := not LPos.Trait;
  LMen := FCoups(LPos) or FCoupsPotentielsPion(LPos);
  LColDepRoi := Colonne(APos.CaseRoi[APos.Trait]);
  LColDepTour := APos.Roque[APos.Trait].XTourRoi;
  if (LColDepTour >= 0) and (LColDepTour <= 7) then
    Recherche(CColonneG, CColonneF);
  LColDepTour := APos.Roque[APos.Trait].XTourDame;
  if (LColDepTour >= 0) and (LColDepTour <= 7) then
    Recherche(CColonneC, CColonneD);
end;

end.
