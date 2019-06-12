
{**
  @abstract(Déplacement des pièces.)
  Complément de l'unité échecs.
}

unit Deplacement;

interface

uses
  Damier, Echecs;

function TypePiece(const APos: TPosition; const AIdx: integer): TPieceEtendu;
{** Met à jour la position en fonction d'un coup présumé légal. Renvoie FALSE si une impossibilité de jouer le coup est détectée. }
function FRejoue(var APos: TPosition; const ACoup: string): boolean;
function EstUnePromotion(const APos: TPosition; const ACoup: string): boolean;
function EstUnRoque(const APos: TPosition; const ACoup: integer): boolean;
procedure Reformule(var ARoque: integer);

implementation

uses
  SysUtils, Tables, Journal;

function TypePiece(const APos: TPosition; const AIdx: integer): TPieceEtendu;
begin
  if      EstAllumeeIdx(APos.Pions,     AIdx) then
    if APos.Trait then
    result := PionNoir else
    result := PionBlanc
  else if EstAllumeeIdx(APos.Tours,     AIdx) then
    result := Tour
  else if EstAllumeeIdx(APos.Cavaliers, AIdx) then
    result := Cavalier
  else if EstAllumeeIdx(APos.Fous,      AIdx) then
    result := Fou
  else if EstAllumeeIdx(APos.Dames,     AIdx) then
    result := Dame
  else if EstAllumeeIdx(APos.Rois,      AIdx) then
    result := Roi
  else
    result := Neant;
end;

function FRejoue(var APos: TPosition; const ACoup: string): boolean;
var
  LDep, LArr, LColDep, LColArr, LLigDep, LLigArr, LPris: integer;
  LType: TPiece;
  LPreserveCouleur: boolean;
begin
  result := TRUE;
  
  { Conversion de la chaîne en index des cases de départ et d'arrivée. L'index est un nombre de 0 à 63. }
  LDep := DecodeNomCase(Copy(ACoup, 1, 2));
  LArr := DecodeNomCase(Copy(ACoup, 3, 2));
  
  Assert(EstAllumeeIdx(APos.PiecesCouleur[APos.Trait], LDep), 'Impossible de déterminer la couleur de la pièce.');
  
  if EstAllumeeIdx(APos.Pions, LDep) then
  begin
    if APos.Trait then
      LType := PionNoir
    else
      LType := PionBlanc;
  end
  else if EstAllumeeIdx(APos.Tours, LDep) then
    LType := Tour
  else if EstAllumeeIdx(APos.Cavaliers, LDep) then
    LType := Cavalier
  else if EstAllumeeIdx(APos.Fous, LDep) then
    LType := Fou
  else if EstAllumeeIdx(APos.Dames, LDep) then
    LType := Dame
  else if EstAllumeeIdx(APos.Rois, LDep) then
    LType := Roi
  else
    Assert(FALSE, 'Impossible de déterminer le type de la pièce.');

  LColDep := LDep mod 8;
  LColArr := LArr mod 8;
  LLigDep := LDep div 8;
  LLigArr := LArr div 8;
  LPreserveCouleur := FALSE;
  
  { Si la pièce déplacée est un roi... }
  if LType = Roi then
  begin
    if EstAllumeeIdx(APos.Tours and {LActives}APos.PiecesCouleur[APos.Trait], LArr) then
    begin
      
      if LColArr = APos.Roque[APos.Trait].XTourRoi then
      begin
        TJournal.Ajoute(Format('[FRejoue] Roque 960 roi %s.', [ACoup]));
        DeplaceIdx(APos.Tours, APos.PiecesCouleur[APos.Trait], LArr, CATCR[APos.Trait]);
        LArr := FIndex(CColonneG, LLigArr);
        LPreserveCouleur := LColDep = CATCR[APos.Trait] mod 8;
      end else
        if LColArr = APos.Roque[APos.Trait].XTourDame then
        begin
          TJournal.Ajoute(Format('[FRejoue] Roque 960 dame %s.', [ACoup]));
          DeplaceIdx(APos.Tours, APos.PiecesCouleur[APos.Trait], LArr, CATCD[APos.Trait]);
          LArr := FIndex(CColonneC, LLigArr);
          LPreserveCouleur := LColDep = CATCD[APos.Trait] mod 8;
        end else
          exit(FALSE);
      
    end else
      if Abs(LColArr - LColDep) = 2 then
      begin
        if LColArr = CColonneG then
        begin
          TJournal.Ajoute(Format('[FRejoue] Roque roi %s.', [ACoup]));
          DeplaceIdx(APos.Tours, APos.PiecesCouleur[APos.Trait], CDTCR[APos.Trait], CATCR[APos.Trait]);
        end else
          if LColArr = CColonneC then
          begin
            TJournal.Ajoute(Format('[FRejoue] Roque dame %s.', [ACoup]));
            DeplaceIdx(APos.Tours, APos.PiecesCouleur[APos.Trait], CDTCD[APos.Trait], CATCD[APos.Trait]);
          end else
            exit(FALSE);
      end;
    
    APos.Roque[APos.Trait].XTourRoi := CNeant;
    APos.Roque[APos.Trait].XTourDame := CNeant;
    APos.PositionRoi[APos.Trait] := CCaseIdx[LArr];
  end;
  
  { Si la pièce déplacée est une tour... }
  if LType = Tour then
    with APos.Roque[APos.Trait] do
      if LColDep = XTourRoi then
        XTourRoi := CNeant
      else
      if LColDep = XTourDame then
        XTourDame := CNeant;
  
  { S'il y a une pièce sur la case d'arrivée... }
  if EstAllumeeIdx({LPassives}APos.PiecesCouleur[not APos.Trait], LArr) then
  begin
    if EstAllumeeIdx(APos.Tours, LArr)
    and (LLigArr = CLigneRoque[not APos.Trait]) then
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
    EteintIdx(APos.PiecesCouleur[not APos.Trait], LArr);
  end;
  
  { Si la pièce déplacée est un pion... }
  if (LType = PionBlanc) or (LType = PionNoir) then
  begin
    { Promotion. }
    if (Length(ACoup) = 4) and EstUnePromotion(APos, ACoup) then
    begin
      EteintIdx(APos.Pions, LDep);
      AllumeIdx(APos.Dames, LDep);
      LType := Dame;
    end else
      if (Length(ACoup) = 5) then
        case ACoup[5] of
          'n':
            begin
              EteintIdx(APos.Pions, LDep);
              AllumeIdx(APos.Cavaliers, LDep);
              LType := Cavalier;
            end;
          'b':
            begin
              EteintIdx(APos.Pions, LDep);
              AllumeIdx(APos.Fous, LDep);
              LType := Fou;
            end;
          'r':
            begin
              EteintIdx(APos.Pions, LDep);
              AllumeIdx(APos.Tours, LDep);
              LType := Tour;
            end;
          'q':
            begin
              EteintIdx(APos.Pions, LDep);
              AllumeIdx(APos.Dames, LDep);
              LType := Dame;
            end;
          else
            TJournal.Ajoute(Format('Valeur inattendue %s.', [ACoup[5]]));    
        end;
    
    { Prise en passant. }
    if LArr = APos.EnPassant then
    begin
      LPris := FIndex(LColArr, LLigDep);
      EteintIdx(APos.Pions, LPris);
      EteintIdx(APos.PiecesCouleur[not APos.Trait], LPris);
    end;
  end;
  
  if ((LType = PionBlanc) or (LType = PionNoir)) and (Abs(LLigArr - LLigDep) = 2) then
    APos.EnPassant := FIndex(LColDep, LLigDep + (LLigArr - LLigDep) div 2)
  else
    APos.EnPassant := CNeant;
  
  { Déplacement de la pièce. }
  case LType of
    PionBlanc,
    PionNoir: DeplaceIdx(APos.Pions,     APos.PiecesCouleur[APos.Trait], LDep, LArr, LPreserveCouleur);
    Tour:     DeplaceIdx(APos.Tours,     APos.PiecesCouleur[APos.Trait], LDep, LArr, LPreserveCouleur);
    Cavalier: DeplaceIdx(APos.Cavaliers, APos.PiecesCouleur[APos.Trait], LDep, LArr, LPreserveCouleur);
    Fou:      DeplaceIdx(APos.Fous,      APos.PiecesCouleur[APos.Trait], LDep, LArr, LPreserveCouleur);
    Dame:     DeplaceIdx(APos.Dames,     APos.PiecesCouleur[APos.Trait], LDep, LArr, LPreserveCouleur);
    Roi:      DeplaceIdx(APos.Rois,      APos.PiecesCouleur[APos.Trait], LDep, LArr, LPreserveCouleur);
  end;
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
    not APos.Trait and (LArr = CLigne8)
    or  APos.Trait and (LArr = CLigne1)
  );
end;

function EstUnRoque(const APos: TPosition; const ACoup: integer): boolean;
var
  LDep, LArr: integer;
begin
  LDep := ACoup div 100;
  LArr := ACoup mod 100;
  result :=
    (EstAllumeeIdx(APos.PiecesCouleur[FALSE], LDep) and EstAllumeeIdx(APos.PiecesCouleur[FALSE], LArr)) or
    (EstAllumeeIdx(APos.PiecesCouleur[TRUE], LDep) and EstAllumeeIdx(APos.PiecesCouleur[TRUE], LArr));
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
  Assert((LLigArr = LLigDep) and ((LLigDep = CLigne1) or (LLigDep = CLigne8)));
  if LArr mod 8 > LDep mod 8 then
    LColArr := CColonneG
  else
    LColArr := CColonneC;
  LArr := 8 * LLigArr + LColArr;
  ARoque := EncodeCoup(LDep, LArr);
  TJournal.Ajoute(Format('[Reformule] Reformulé %s en %s.', [LAncienNom, Concat(CNomCase[LDep], CNomCase[LArr])]));
end;

end.
