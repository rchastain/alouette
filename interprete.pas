
{**
@abstract(Déplacement des pièces.)
Complément de l'unité échecs.
}

unit Interprete;

interface

uses
  Echecs;

{** Met à jour la position en fonction d'un coup présumé légal. Renvoie FALSE si une impossibilité de jouer le coup est détectée. }
function Rejoue_(var APos: TPosition; const ACoup: string): boolean;
function EstUnePromotion(const APos: TPosition; const ACoup: string): boolean;
function EstUnRoque(const APos: TPosition; const ACoup: integer): boolean;
procedure Reformule(var ARoque: integer);

implementation

uses
  SysUtils, Damier, Tables, Journal;
  
function Rejoue_(var APos: TPosition; const ACoup: string): boolean;
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
  and EstAllumeeIndex(APos.Blanches, LDep) then
  begin
    LCouleur := @APos.Blanches;
    LAdv := @APos.Noires;
  end else
  if (APos.Trait = CNoir)
  and EstAllumeeIndex(APos.Noires, LDep) then
  begin
    LCouleur := @APos.Noires;
    LAdv := @APos.Blanches;
  end else
    exit(FALSE);
  
  { Pointeur vers le damier contenant la position des pièces de ce type. }
  if EstAllumeeIndex(APos.Pions, LDep) then
    LType := @APos.Pions
  else if EstAllumeeIndex(APos.Tours, LDep) then
    LType := @APos.Tours
  else if EstAllumeeIndex(APos.Cavaliers, LDep) then
    LType := @APos.Cavaliers
  else if EstAllumeeIndex(APos.Fous, LDep) then
    LType := @APos.Fous
  else if EstAllumeeIndex(APos.Dames, LDep) then
    LType := @APos.Dames
  else if EstAllumeeIndex(APos.Rois, LDep) then
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
    if EstAllumeeIndex(APos.Tours and LCouleur^, LArr) then
    begin
      
      if LColArr = APos.Roque[APos.Trait].XTourRoi then
      begin
        TJournal.Ajoute(Format('[Rejoue_] Roque 960 roi %s.', [ACoup]));
        DeplaceIndex(APos.Tours, LCouleur^, LArr, CATCR[APos.Trait]);
        LArr := FIndex(CColG, LLigArr);
        LPreserveCouleur := LColDep = CATCR[APos.Trait] mod 8;
      end else
        if LColArr = APos.Roque[APos.Trait].XTourDame then
        begin
          TJournal.Ajoute(Format('[Rejoue_] Roque 960 dame %s.', [ACoup]));
          DeplaceIndex(APos.Tours, LCouleur^, LArr, CATCD[APos.Trait]);
          LArr := FIndex(CColC, LLigArr);
          LPreserveCouleur := LColDep = CATCD[APos.Trait] mod 8;
        end else
          exit(FALSE);
      
    end else
      if Abs(LColArr - LColDep) = 2 then
      begin
        if LColArr = CColG then
        begin
          TJournal.Ajoute(Format('[Rejoue_] Roque roi %s.', [ACoup]));
          DeplaceIndex(APos.Tours, LCouleur^, CDTCR[APos.Trait], CATCR[APos.Trait]);
        end else
          if LColArr = CColC then
          begin
            TJournal.Ajoute(Format('[Rejoue_] Roque dame %s.', [ACoup]));
            DeplaceIndex(APos.Tours, LCouleur^, CDTCD[APos.Trait], CATCD[APos.Trait]);
          end else
            exit(FALSE);
      end;
    
    APos.Roque[APos.Trait].XTourRoi := CNeant;
    APos.Roque[APos.Trait].XTourDame := CNeant;
    APos.PositionRoi[APos.Trait] := CCaseIndex[LArr];
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
  if EstAllumeeIndex(LAdv^, LArr) then
  begin
    if EstAllumeeIndex(APos.Tours, LArr)
    and (LLigArr = CLigRoq[not APos.Trait]) then
      with APos.Roque[not APos.Trait] do
        if (LColArr = XTourRoi) then
          XTourRoi := CNeant
        else
        if LColArr = XTourRoi then
          XTourRoi := CNeant;
        
    with APos do
    begin
      EteintIndex(Pions, LArr);
      EteintIndex(Tours, LArr);
      EteintIndex(Cavaliers, LArr);
      EteintIndex(Fous, LArr);
      EteintIndex(Dames, LArr);
      EteintIndex(Rois, LArr);
    end;
    EteintIndex(LAdv^, LArr);
  end;
  
  { Si la pièce déplacée est un pion... }
  if (LType = @APos.Pions) then
  begin
    { Promotion. }
    if (Length(ACoup) = 5) then
      case ACoup[5] of
        'n':
          begin
            EteintIndex(LType^, LDep);
            AllumeIndex(APos.Cavaliers, LDep);
            LType := @APos.Cavaliers;
          end;
        'b':
          begin
            EteintIndex(LType^, LDep);
            AllumeIndex(APos.Fous, LDep);
            LType := @APos.Fous;
          end;
        'r':
          begin
            EteintIndex(LType^, LDep);
            AllumeIndex(APos.Tours, LDep);
            LType := @APos.Tours;
          end;
        'q':
          begin
            EteintIndex(LType^, LDep);
            AllumeIndex(APos.Dames, LDep);
            LType := @APos.Dames;
          end;
        else
          TJournal.Ajoute(Format('Valeur inattendue %s.', [ACoup[5]]));    
    end;
    
    { Prise en passant. }
    if LArr = APos.EnPassant then
    begin
      LPris := FIndex(LColArr, LLigDep);
      EteintIndex(APos.Pions, LPris);
      EteintIndex(LAdv^, LPris);
    end;
  end;
  
  if (LType = @APos.Pions) and (Abs(LLigArr - LLigDep) = 2) then
    APos.EnPassant := FIndex(LColDep, LLigDep + (LLigArr - LLigDep) div 2)
  else
    APos.EnPassant := CNeant;
  
  { Déplacement de la pièce. }
  DeplaceIndex(LType^, LCouleur^, LDep, LArr, LPreserveCouleur);
  { Changement du trait. }
  APos.Trait := not APos.Trait;
end;

function EstUnePromotion(const APos: TPosition; const ACoup: string): boolean;
var
  LDep, LArr: integer;
begin
  LDep := DecodeNomCase(Copy(ACoup, 1, 2));
  LArr := DecodeNomCase(Copy(ACoup, 3, 2)) div 8;
  result := EstAllumeeIndex(APos.Pions, LDep) and (
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
    (EstAllumeeIndex(APos.Blanches, LDep) and EstAllumeeIndex(APos.Blanches, LArr)) or
    (EstAllumeeIndex(APos.Noires, LDep) and EstAllumeeIndex(APos.Noires, LArr));
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
