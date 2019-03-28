
unit Coups;

interface

uses
  Echecs, Damier;

function GenereCoups(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ACourt: boolean = FALSE): TDamier; overload;
{** Renvoie un damier représentant les cases menacées par un coup de l'adversaire. Les coups ne sont pas conservés. }
function GenereCoups(const APos: TPosition): TDamier; overload;

implementation

uses
  SysUtils, Tables;

function GenereCoups(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ACourt: boolean): TDamier;
var
  LCompte: integer = 0;
procedure Accepte(const i, j: integer; const ACondition: boolean = TRUE);
begin
  if ACondition then
  begin
    Allume(result, CCase[j]);
    if ACourt then
      exit;
    Inc(LCompte);
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
  
  result := CDamierVide;
  
  for i := A1 to H8 do if Allumee(actives, CCase[i]) then
  begin
    { Pion. }
    if Allumee(APos.Pions, CCase[i]) then
    begin
      { Pas en avant. }
      k := 8 - 16 * Ord(APos.Trait);
      j := i + k;
      if not Allumee(toutes, CCase[j]) then
      begin
        Accepte(i, j);
        { Second pas en avant. }
        if ((j div 8 = 2) and not APos.Trait)
        or ((j div 8 = 5) and APos.Trait) then
        begin
          j := j + k;
          Accepte(i, j, not Allumee(toutes, CCase[j]));
        end;
      end;
      { Prise côté dame. }
      if i mod 8 > 0 then
      begin
        j := Pred(i + k);
        Accepte(i, j,
          Allumee(passives, CCase[j])
          xor (j = APos.EnPassant)
        );
      end;
      { Prise côté roi. }
      if i mod 8 < 7 then
      begin
        j := Succ(i + k);
        Accepte(i, j,
          Allumee(passives, CCase[j])
          xor (j = APos.EnPassant)
        );
      end;
    end else
    { Tour. }
    if Allumee(APos.Tours, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          Allumee(CCibles[Tour, i], CCase[j])
          and (not Allumee(actives, CCase[j]))
          and ((CChemin[i, j] and toutes) = 0)
        );
    end else
    { Cavalier. }
    if Allumee(APos.Cavaliers, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          Allumee(CCibles[Cavalier, i], CCase[j])
          and not Allumee(actives, CCase[j])
        );
    end else
    { Fou. }
    if Allumee(APos.Fous, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          Allumee(CCibles[Fou, i], CCase[j])
          and (not Allumee(actives, CCase[j]))
          and ((CChemin[i, j] and toutes) = 0)
        );
    end else
    { Dame. }
    if Allumee(APos.Dames, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          Allumee(CCibles[Dame, i], CCase[j])
          and (not Allumee(actives, CCase[j]))
          and ((CChemin[i, j] and toutes) = 0)
        );
    end else
    { Roi. }
    if Allumee(APos.Rois, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          Allumee(CCibles[Roi, i], CCase[j])
          and not Allumee(actives, CCase[j])
        );
    end;
  end;
  ACompte := LCompte;
end;

function GenereCoups(const APos: TPosition): TDamier;
var
  LListe: array[0..0] of integer;
  ACompte: integer;
begin
  result := GenereCoups(APos, LListe, ACompte, TRUE);
end;

end.
