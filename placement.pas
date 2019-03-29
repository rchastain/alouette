
{**
@abstract(Déplacement des pièces.)
Complément de l'unité échecs.
}

unit Placement;

interface

uses
  Echecs;

{** Met à jour la position en fonction d'un coup joué. Renvoie FALSE si le coup ne peut pas être joué. }
function Rejoue_(var APos: TPosition; const ACoup: string): boolean;
function EstUnePromotion(const APos: TPosition; const ACoup: string): boolean;

implementation

uses
  SysUtils, Damier, Tables, Journal;
  
function Rejoue_(var APos: TPosition; const ACoup: string): boolean;
procedure Avertit(const ALigne: string);
begin
  TJournal.Ajoute(Format('Impossible de jouer le coup "%s". Unité %s, ligne %s.', [ACoup, {$I %FILE%}, ALigne]));
end;
type
  TCaseRoque = array[boolean] of integer;
const
  { Arrivée tour, côté roi. }
  CATCR: TCaseRoque = (F1, F8);
  { Arrivée tour, côté dame. }
  CATCD: TCaseRoque = (D1, D8);
  { Départ tour, côté roi. }
  CDTCR: TCaseRoque = (H1, H8);
  { Depart tour, côté dame. }
  CDTCD: TCaseRoque = (A1, A8);
var
  LDep, LArr: integer;
  LType, LCouleur, LAdversaire: ^TDamier;
begin
  result := TRUE;
  { Conversion de la chaîne en index des cases de départ et d'arrivée. L'index est un nombre de 0 à 63. }
  LDep := DecodeNomCase(Copy(ACoup, 1, 2));
  LArr := DecodeNomCase(Copy(ACoup, 3, 2));
  { On assigne une valeur aux pointeurs en fonction du trait et dans la foulée on vérifie qu'il y a bien une pièce de la bonne couleur sur la case de départ. }
  if (APos.Trait = CBlanc)
  and Allumee(APos.Blanches, CCase[LDep]) then
  begin
    LCouleur := @APos.Blanches;
    LAdversaire := @APos.Noires;
  end else
  if (APos.Trait = CNoir)
  and Allumee(APos.Noires, CCase[LDep]) then
  begin
    LCouleur := @APos.Noires;
    LAdversaire := @APos.Blanches;
  end else
  begin
    Avertit({$I %LINE%});
    exit(FALSE);
  end;
  
  { Pointeur vers le damier contenant la position des pièces de ce type. }
  if Allumee(APos.Pions, CCase[LDep]) then
    LType := @APos.Pions
  else if Allumee(APos.Tours, CCase[LDep]) then
    LType := @APos.Tours
  else if Allumee(APos.Cavaliers, CCase[LDep]) then
    LType := @APos.Cavaliers
  else if Allumee(APos.Fous, CCase[LDep]) then
    LType := @APos.Fous
  else if Allumee(APos.Dames, CCase[LDep]) then
    LType := @APos.Dames
  else if Allumee(APos.Rois, CCase[LDep]) then
    LType := @APos.Rois
  else
  begin
    Avertit({$I %LINE%});
    exit(FALSE);
  end;
  
  { Si la pièce déplacée est un roi... }
  if LType = @APos.Rois then
  begin
    if Allumee(APos.Tours and LCouleur^, CCase[LArr]) then
    begin
      if LArr mod 8 = APos.Roque[APos.Trait].XTourRoi then
      begin
        TJournal.Ajoute(Format('Roque échecs 960 côté roi (%s).', [ACoup]));
        Deplace(APos.Tours, LCouleur^, CCase[LArr], CCase[CATCR[APos.Trait]]);
        LArr := FIndex(6, LArr div 8);
      end else
        if LArr mod 8 = APos.Roque[APos.Trait].XTourDame then
        begin
          TJournal.Ajoute(Format('Roque échecs 960 côté dame (%s).', [ACoup]));
          Deplace(APos.Tours, LCouleur^, CCase[LArr], CCase[CATCD[APos.Trait]]);
          LArr := FIndex(2, LArr div 8);
        end else
          begin
            Avertit({$I %LINE%});
            exit(FALSE);
          end;
    end else
      if (Abs((LArr mod 8) - (LDep mod 8)) = 2) then
      begin
        if LArr mod 8 = 6 then
        begin
          TJournal.Ajoute(Format('Roque vieux échecs côté roi (%s).', [ACoup]));
          Deplace(APos.Tours, LCouleur^, CCase[CDTCR[APos.Trait]], CCase[CATCR[APos.Trait]]);
        end else
          if LArr mod 8 = 2 then
          begin
            TJournal.Ajoute(Format('Roque vieux échecs côté dame (%s).', [ACoup]));
            Deplace(APos.Tours, LCouleur^, CCase[CDTCD[APos.Trait]], CCase[CATCD[APos.Trait]]);
          end else
          begin
            Avertit({$I %LINE%});
            exit(FALSE);
          end; 
      end;
    APos.Roque[APos.Trait].XTourRoi := CIndisponible;
    APos.Roque[APos.Trait].XTourDame := CIndisponible;
  end;
  
  { Si la pièce déplacée est une tour... }
  if LType = @APos.Tours then
    if LDep mod 8 = APos.Roque[APos.Trait].XTourRoi then
      APos.Roque[APos.Trait].XTourRoi := CIndisponible
    else
    if LDep mod 8 = APos.Roque[APos.Trait].XTourDame then
      APos.Roque[APos.Trait].XTourDame := CIndisponible;
  
  { S'il y a une pièce sur la case d'arrivée. }
  if Allumee(LAdversaire^, CCase[LArr]) then
  begin
    with APos do
    begin
      Eteint(Pions, CCase[LArr]);
      Eteint(Tours, CCase[LArr]);
      Eteint(Cavaliers, CCase[LArr]);
      Eteint(Fous, CCase[LArr]);
      Eteint(Dames, CCase[LArr]);
      Eteint(Rois, CCase[LArr]);
    end;
    LAdversaire^ := LAdversaire^ and not CCase[LArr];
  end;
  
  { Si la pièce déplacée est un pion... }
  if (LType = @APos.Pions) then
  begin
    { Promotion. }
    if (Length(ACoup) = 5) then
      case ACoup[5] of
        'n':
          begin
            LType^ := LType^ and not CCase[LDep];
            APos.Cavaliers := APos.Cavaliers or CCase[LDep];
            LType := @APos.Cavaliers;
          end;
        'b':
          begin
            LType^ := LType^ and not CCase[LDep];
            APos.Fous := APos.Fous or CCase[LDep];
            LType := @APos.Fous;
          end;
        'r':
          begin
            LType^ := LType^ and not CCase[LDep];
            APos.Tours := APos.Tours or CCase[LDep];
            LType := @APos.Tours;
          end;
        'q':
          begin
            LType^ := LType^ and not CCase[LDep];
            APos.Dames := APos.Dames or CCase[LDep];
            LType := @APos.Dames;
          end;
        else
          TJournal.Ajoute(Format('Valeur inattendue %s.', [ACoup[5]]));    
    end;
    
    { Prise en passant. }
    if LArr = APos.EnPassant then
    begin
      Eteint(APos.Pions, CCase[LArr]);
      Eteint(LAdversaire^, CCase[LArr]);
    end;
  end;
  { Déplacement de la pièce. }
  Deplace(LType^, LCouleur^, CCase[LDep], CCase[LArr]);
  { Changement du trait. }
  APos.Trait := not APos.Trait;
end;

function EstUnePromotion(const APos: TPosition; const ACoup: string): boolean;
var
  LDep, LArr: integer;
begin
  LDep := DecodeNomCase(Copy(ACoup, 1, 2));
  LArr := DecodeNomCase(Copy(ACoup, 3, 2));
  result := Allumee(APos.Pions, CCase[LDep]) and (
    not APos.Trait and (LArr div 8 = 7)
    or APos.Trait and (LArr div 8 = 0)
  );
end;

end.
