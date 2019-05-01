
{**
  @abstract(Déplacement des pièces.)
  Complément de l'unité échecs.
}

unit Deplacement;

interface

uses
  Echecs;

{** Met à jour la position en fonction d'un coup présumé légal. Renvoie FALSE si une impossibilité de jouer le coup est détectée. }
function FRejoue(var APos: TPosition; const ACoup: string): boolean;
function EstUnePromotion(const APos: TPosition; const ACoup: string): boolean;
function EstUnRoque(const APos: TPosition; const ACoup: integer): boolean;
procedure Reformule(var ARoque: integer);

implementation

uses
  SysUtils, Damier, Tables, Journal;
  
function FRejoue(var APos: TPosition; const ACoup: string): boolean;
var
  LDep, LArr, LColDep, LColArr, LLigDep, LLigArr, LPris: integer;
  LType, LCouleur, LAdv: ^TDamier;
  LPreserveCouleur: boolean;
begin
  result := TRUE;
  { Conversion de la chaîne en index des cases de départ et d'arrivée. L'index est un nombre de 0 à 63. }
  LDep := DecodeNomCase(Copy(ACoup, 1, 2));
  LArr := DecodeNomCase(Copy(ACoup, 3, 2));
  { On assigne une valeur aux pointeurs en fonction du trait et dans la foulée on vérifie qu'il y a bien une pièce de la bonne couleur sur la case de départ. }
  if (APos.Trait = CBlanc)
  and EstAllumeeIdx(APos.Blanches, LDep) then
  begin
    LCouleur := @APos.Blanches;
    LAdv := @APos.Noires;
  end else
  if (APos.Trait = CNoir)
  and EstAllumeeIdx(APos.Noires, LDep) then
  begin
    LCouleur := @APos.Noires;
    LAdv := @APos.Blanches;
  end else
    exit(FALSE);
  
  { Pointeur vers le damier contenant la position des pièces de ce type. }
  if EstAllumeeIdx(APos.Pions, LDep) then
    LType := @APos.Pions
  else if EstAllumeeIdx(APos.Tours, LDep) then
    LType := @APos.Tours
  else if EstAllumeeIdx(APos.Cavaliers, LDep) then
    LType := @APos.Cavaliers
  else if EstAllumeeIdx(APos.Fous, LDep) then
    LType := @APos.Fous
  else if EstAllumeeIdx(APos.Dames, LDep) then
    LType := @APos.Dames
  else if EstAllumeeIdx(APos.Rois, LDep) then
    LType := @APos.Rois
  else
    exit(FALSE);

  LColDep := LDep mod 8;
  LColArr := LArr mod 8;
  LLigDep := LDep div 8;
  LLigArr := LArr div 8;
  LPreserveCouleur := FALSE;
  
  { Si la pièce déplacée est un roi... }
  if LType = @APos.Rois then
  begin
    if EstAllumeeIdx(APos.Tours and LCouleur^, LArr) then
    begin
      
      if LColArr = APos.Roque[APos.Trait].XTourRoi then
      begin
        TJournal.Ajoute(Format('[FRejoue] Roque 960 roi %s.', [ACoup]));
        DeplaceIdx(APos.Tours, LCouleur^, LArr, CATCR[APos.Trait]);
        LArr := FIndex(CColG, LLigArr);
        LPreserveCouleur := LColDep = CATCR[APos.Trait] mod 8;
      end else
        if LColArr = APos.Roque[APos.Trait].XTourDame then
        begin
          TJournal.Ajoute(Format('[FRejoue] Roque 960 dame %s.', [ACoup]));
          DeplaceIdx(APos.Tours, LCouleur^, LArr, CATCD[APos.Trait]);
          LArr := FIndex(CColC, LLigArr);
          LPreserveCouleur := LColDep = CATCD[APos.Trait] mod 8;
        end else
          exit(FALSE);
      
    end else
      if Abs(LColArr - LColDep) = 2 then
      begin
        if LColArr = CColG then
        begin
          TJournal.Ajoute(Format('[FRejoue] Roque roi %s.', [ACoup]));
          DeplaceIdx(APos.Tours, LCouleur^, CDTCR[APos.Trait], CATCR[APos.Trait]);
        end else
          if LColArr = CColC then
          begin
            TJournal.Ajoute(Format('[FRejoue] Roque dame %s.', [ACoup]));
            DeplaceIdx(APos.Tours, LCouleur^, CDTCD[APos.Trait], CATCD[APos.Trait]);
          end else
            exit(FALSE);
      end;
    
    APos.Roque[APos.Trait].XTourRoi := CNeant;
    APos.Roque[APos.Trait].XTourDame := CNeant;
    APos.PositionRoi[APos.Trait] := CCaseIdx[LArr];
  end;
  
  { Si la pièce déplacée est une tour... }
  if LType = @APos.Tours then
    with APos.Roque[APos.Trait] do
      if LColDep = XTourRoi then
        XTourRoi := CNeant
      else
      if LColDep = XTourDame then
        XTourDame := CNeant;
  
  { S'il y a une pièce sur la case d'arrivée... }
  if EstAllumeeIdx(LAdv^, LArr) then
  begin
    if EstAllumeeIdx(APos.Tours, LArr)
    and (LLigArr = CLigRoq[not APos.Trait]) then
      with APos.Roque[not APos.Trait] do
        if (LColArr = XTourRoi) then
          XTourRoi := CNeant
        else
        if LColArr = XTourRoi then
          XTourRoi := CNeant;
        
    with APos do
    begin
      EteintIdx(Pions, LArr);
      EteintIdx(Tours, LArr);
      EteintIdx(Cavaliers, LArr);
      EteintIdx(Fous, LArr);
      EteintIdx(Dames, LArr);
      EteintIdx(Rois, LArr);
    end;
    EteintIdx(LAdv^, LArr);
  end;
  
  { Si la pièce déplacée est un pion... }
  if (LType = @APos.Pions) then
  begin
    { Promotion. }
    if (Length(ACoup) = 4) and EstUnePromotion(APos, ACoup) then
    begin
      EteintIdx(LType^, LDep);
      AllumeIdx(APos.Dames, LDep);
      LType := @APos.Dames;
    end else
      if (Length(ACoup) = 5) then
        case ACoup[5] of
          'n':
            begin
              EteintIdx(LType^, LDep);
              AllumeIdx(APos.Cavaliers, LDep);
              LType := @APos.Cavaliers;
            end;
          'b':
            begin
              EteintIdx(LType^, LDep);
              AllumeIdx(APos.Fous, LDep);
              LType := @APos.Fous;
            end;
          'r':
            begin
              EteintIdx(LType^, LDep);
              AllumeIdx(APos.Tours, LDep);
              LType := @APos.Tours;
            end;
          'q':
            begin
              EteintIdx(LType^, LDep);
              AllumeIdx(APos.Dames, LDep);
              LType := @APos.Dames;
            end;
          else
            TJournal.Ajoute(Format('Valeur inattendue %s.', [ACoup[5]]));    
        end;
    
    { Prise en passant. }
    if LArr = APos.EnPassant then
    begin
      LPris := FIndex(LColArr, LLigDep);
      EteintIdx(APos.Pions, LPris);
      EteintIdx(LAdv^, LPris);
    end;
  end;
  
  if (LType = @APos.Pions) and (Abs(LLigArr - LLigDep) = 2) then
    APos.EnPassant := FIndex(LColDep, LLigDep + (LLigArr - LLigDep) div 2)
  else
    APos.EnPassant := CNeant;
  
  { Déplacement de la pièce. }
  DeplaceIdx(LType^, LCouleur^, LDep, LArr, LPreserveCouleur);
  { Changement du trait. }
  APos.Trait := not APos.Trait;
end;

function EstUnePromotion(const APos: TPosition; const ACoup: string): boolean;
var
  LDep, LArr: integer;
begin
  LDep := DecodeNomCase(Copy(ACoup, 1, 2));
  LArr := DecodeNomCase(Copy(ACoup, 3, 2)) div 8;
  result := EstAllumeeIdx(APos.Pions, LDep) and (
    not APos.Trait and (LArr = CLig8)
    or  APos.Trait and (LArr = CLig1)
  );
end;

function EstUnRoque(const APos: TPosition; const ACoup: integer): boolean;
var
  LDep, LArr: integer;
begin
  LDep := ACoup div 100;
  LArr := ACoup mod 100;
  result :=
    (EstAllumeeIdx(APos.Blanches, LDep) and EstAllumeeIdx(APos.Blanches, LArr)) or
    (EstAllumeeIdx(APos.Noires, LDep) and EstAllumeeIdx(APos.Noires, LArr));
  if result then
    TJournal.Ajoute(Format('[EstUnRoque] Roque détecté %s.', [NomCoup(ACoup)]));
end;

procedure Reformule(var ARoque: integer);
var
  LDep, LArr, LLigDep, LLigArr, LColArr: integer;
  LAncienNom: string;
begin
  LDep := ARoque div 100;
  LArr := ARoque mod 100;
  Assert((LDep >= 0) and (LDep <= 63) and (LArr >= 0) and (LArr <= 63));
  LAncienNom := Concat(CNomCase[LDep], CNomCase[LArr]);
  LLigDep := LDep div 8;
  LLigArr := LArr div 8;
  Assert((LLigArr = LLigDep) and ((LLigDep = CLig1) or (LLigDep = CLig8)));
  if LArr mod 8 > LDep mod 8 then
    LColArr := CColG
  else
    LColArr := CColC;
  LArr := 8 * LLigArr + LColArr;
  ARoque := EncodeCoup(LDep, LArr);
  TJournal.Ajoute(Format('[Reformule] Reformulé %s en %s.', [LAncienNom, Concat(CNomCase[LDep], CNomCase[LArr])]));
end;

end.
