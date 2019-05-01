
{**
  @abstract(Génération des coups.)
  Génération des coups.
}

unit Coups;

interface

uses
  Echecs, Damier;

function ChercheCoups(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ARapide: boolean = FALSE): TDamier; overload;
{** Renvoie un damier représentant les cases pouvant être atteintes. Les coups ne sont pas conservés. }
function ChercheCoups(const APos: TPosition): TDamier; overload;
function ChercheNombre(const APos: TPosition): integer;
function ChercheProtections(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ARapide: boolean = FALSE): integer; overload;
{** Renvoie un damier représentant les cases pouvant être atteintes. Les coups ne sont pas conservés. }
function ChercheProtections(const APos: TPosition): integer; overload;
function ChercheCoupsPotentielsPion(const APos: TPosition): TDamier;

implementation

uses
  SysUtils, Tables;

function ChercheCoups(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ARapide: boolean): TDamier;
var
  LCompte: integer = 0;
procedure Accepte(const i, j: integer);
begin
  Allume(result, CCaseIndex[j]);
  Inc(LCompte);
  if not ARapide then
  begin
    Assert(LCompte <= Length(AListe));
    AListe[Pred(LCompte)] := EncodeCoup(i, j);
  end;
end;
var
  { Pièces. }
  actives, passives, toutes: TDamier;
  i, j, k: integer;
begin
  if APos.Trait then begin
    actives := APos.Noires;
    passives := APos.Blanches;
  end else begin
    actives := APos.Blanches;
    passives := APos.Noires;
  end;
  with APos do
    toutes := Blanches or Noires;
  
  result := 0; { Damier vide. }
  
  for i := A1 to H8 do if EstAllumee(actives, CCaseIndex[i]) then
  begin
    { Pion. }
    if EstAllumee(APos.Pions, CCaseIndex[i]) then
    begin
      { Pas en avant. }
      k := 8 - 16 * Ord(APos.Trait);
      j := i + k;
      if not EstAllumee(toutes, CCaseIndex[j]) then
      begin
        Accepte(i, j);
        { Second pas en avant. }
        if ((j div 8 = 2) and not APos.Trait)
        or ((j div 8 = 5) and APos.Trait) then
        begin
          j := j + k;
          if not EstAllumee(toutes, CCaseIndex[j]) then
            Accepte(i, j);
        end;
      end;
      { Prise côté dame. }
      if i mod 8 > 0 then
      begin
        j := Pred(i + k);
        if EstAllumee(passives, CCaseIndex[j]) xor (j = APos.EnPassant) then
          Accepte(i, j);
      end;
      { Prise côté roi. }
      if i mod 8 < 7 then
      begin
        j := Succ(i + k);
        if EstAllumee(passives, CCaseIndex[j]) xor (j = APos.EnPassant) then
          Accepte(i, j);
      end;
    end else
    { Tour. }
    if EstAllumee(APos.Tours, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Tour, i], CCaseIndex[j])
        and not EstAllumee(actives, CCaseIndex[j])
        and ((CChemin[i, j] and toutes) = 0) then
          Accepte(i, j);
    end else
    { Cavalier. }
    if EstAllumee(APos.Cavaliers, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Cavalier, i], CCaseIndex[j])
        and not EstAllumee(actives, CCaseIndex[j]) then
          Accepte(i, j);
    end else
    { Fou. }
    if EstAllumee(APos.Fous, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Fou, i], CCaseIndex[j])
        and not EstAllumee(actives, CCaseIndex[j])
        and ((CChemin[i, j] and toutes) = 0) then
          Accepte(i, j);
    end else
    { Dame. }
    if EstAllumee(APos.Dames, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Dame, i], CCaseIndex[j])
        and not EstAllumee(actives, CCaseIndex[j])
        and ((CChemin[i, j] and toutes) = 0) then
          Accepte(i, j);
    end else
    { Roi. }
    if EstAllumee(APos.Rois, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Roi, i], CCaseIndex[j])
        and not EstAllumee(actives, CCaseIndex[j]) then
          Accepte(i, j);
    end;
  end;
  ACompte := LCompte;
end;

function ChercheCoups(const APos: TPosition): TDamier;
var
  LListe: array[0..0] of integer;
  LCompte: integer;
begin
  result := ChercheCoups(APos, LListe, LCompte, TRUE);
end;

function ChercheNombre(const APos: TPosition): integer;
var
  LListe: array[0..0] of integer;
begin
  ChercheCoups(APos, LListe, result, TRUE);
end;

function ChercheProtections(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ARapide: boolean): integer;
procedure Accepte(const i, j, k: integer);
begin
  Inc(result, k);
  if not ARapide then
  begin
    Assert(result <= Length(AListe));
    AListe[Pred(result)] := EncodeCoup(i, j);
  end;
end;
var
  { Pièces. }
  actives, passives, toutes: TDamier;
  i, j, k: integer;
begin
  if APos.Trait then begin
    actives := APos.Noires;
    passives := APos.Blanches;
  end else begin
    actives := APos.Blanches;
    passives := APos.Noires;
  end;
  with APos do
    toutes := Blanches or Noires;
  
  result := 0;
  
  for i := A1 to H8 do if EstAllumee(actives, CCaseIndex[i]) then
  begin
    { Pion. }
    if EstAllumee(APos.Pions, CCaseIndex[i]) then
    begin
      { Pas en avant. }
      k := 8 - 16 * Ord(APos.Trait);
      { Prise côté dame. }
      if i mod 8 > 0 then
      begin
        j := Pred(i + k);
        if EstAllumee(actives and APos.Pions, CCaseIndex[j]) then
          Accepte(i, j, 5);
      end;
      { Prise côté roi. }
      if i mod 8 < 7 then
      begin
        j := Succ(i + k);
        if EstAllumee(actives and APos.Pions, CCaseIndex[j]) then
          Accepte(i, j, 5);
      end;
    end else
    { Tour. }
    if EstAllumee(APos.Tours, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Tour, i], CCaseIndex[j])
        and EstAllumee(actives, CCaseIndex[j])
        and ((CChemin[i, j] and toutes) = 0) then
          if EstAllumee(APos.Tours, CCaseIndex[j]) then
            Accepte(i, j, 1)
          else
            if EstAllumee(APos.Fous, CCaseIndex[j])
            or EstAllumee(APos.Cavaliers, CCaseIndex[j])
            or EstAllumee(APos.Pions, CCaseIndex[j]) then
              Accepte(i, j, 2);
    end else
    { Cavalier. }
    if EstAllumee(APos.Cavaliers, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Cavalier, i], CCaseIndex[j])
        and EstAllumee(actives, CCaseIndex[j]) then
          if EstAllumee(APos.Fous, CCaseIndex[j])
          or EstAllumee(APos.Cavaliers, CCaseIndex[j]) then
            Accepte(i, j, 1)
          else
            if EstAllumee(APos.Pions, CCaseIndex[j]) then
              Accepte(i, j, 2);
    end else
    { Fou. }
    if EstAllumee(APos.Fous, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Fou, i], CCaseIndex[j])
        and EstAllumee(actives, CCaseIndex[j])
        and ((CChemin[i, j] and toutes) = 0) then
          if EstAllumee(APos.Cavaliers, CCaseIndex[j])
          or EstAllumee(APos.Pions, CCaseIndex[j]) then
            Accepte(i, j, 1);
    end else
    { Dame. }
    if EstAllumee(APos.Dames, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Dame, i], CCaseIndex[j])
        and EstAllumee(actives, CCaseIndex[j])
        and ((CChemin[i, j] and toutes) = 0) then
          if EstAllumee(APos.Tours, CCaseIndex[j])
          or EstAllumee(APos.Fous, CCaseIndex[j])
          or EstAllumee(APos.Cavaliers, CCaseIndex[j])
          or EstAllumee(APos.Pions, CCaseIndex[j]) then
            Accepte(i, j, 1);
    end else
    { Roi. }
    if EstAllumee(APos.Rois, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Roi, i], CCaseIndex[j])
        and EstAllumee(actives, CCaseIndex[j]) then
          if EstAllumee(APos.Pions, CCaseIndex[j]) then
            Accepte(i, j, 1);
    end;
  end;
  ACompte := result;
end;

function ChercheProtections(const APos: TPosition): integer;
var
  LListe: array[0..0] of integer;
  LCompte: integer;
begin
  result := ChercheProtections(APos, LListe, LCompte, TRUE);
end;

function ChercheCoupsPotentielsPion(const APos: TPosition): TDamier;
procedure Accepte(const i, j: integer);
begin
  Allume(result, CCaseIndex[j]);
end;
var
  { Pièces. }
  actives, passives, toutes: TDamier;
  i, j, k: integer;
  LPion: TPiece;
begin
  if APos.Trait then begin
    actives := APos.Noires;
    passives := APos.Blanches;
    LPion := PionNoir;
  end else begin
    actives := APos.Blanches;
    passives := APos.Noires;
    LPion := PionBlanc;
  end;
  with APos do
    toutes := Blanches or Noires;
  
  result := 0; { Damier vide. }
  
  for i := A1 to H8 do if EstAllumee(actives, CCaseIndex[i]) then
  begin
    { Pion. }
    if EstAllumee(APos.Pions, CCaseIndex[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[LPion, i], CCaseIndex[j])
        and not EstAllumee(actives, CCaseIndex[j]) then
          Accepte(i, j);
    end;
  end;
end;

end.
