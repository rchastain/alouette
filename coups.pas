
unit Coups;

interface

uses
  Echecs, Damier;

function GenereCoups(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ARapide: boolean = FALSE): TDamier; overload;
{** Renvoie un damier représentant les cases menacées par un coup de l'adversaire. Les coups ne sont pas conservés. }
function GenereCoups(const APos: TPosition): TDamier; overload;
function GenereCoupsNombre(const APos: TPosition): integer;

implementation

uses
  SysUtils, Tables;

function GenereCoups(const APos: TPosition; var AListe: array of integer; out ACompte: integer; const ARapide: boolean): TDamier;
var
  LCompte: integer = 0;
procedure Accepte(const i, j: integer; const ACondition: boolean = TRUE);
begin
  if ACondition then
  begin
    Allume(result, CCase[j]);
    Inc(LCompte);
    if not ARapide then
    begin
      Assert(LCompte <= Length(AListe));
      AListe[Pred(LCompte)] := EncodeCoup(i, j);
    end;
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
  
  for i := A1 to H8 do if EstAllumee(actives, CCase[i]) then
  begin
    { Pion. }
    if EstAllumee(APos.Pions, CCase[i]) then
    begin
      { Pas en avant. }
      k := 8 - 16 * Ord(APos.Trait);
      j := i + k;
      if not EstAllumee(toutes, CCase[j]) then
      begin
        Accepte(i, j);
        { Second pas en avant. }
        if ((j div 8 = 2) and not APos.Trait)
        or ((j div 8 = 5) and APos.Trait) then
        begin
          j := j + k;
          Accepte(i, j, not EstAllumee(toutes, CCase[j]));
        end;
      end;
      { Prise côté dame. }
      if i mod 8 > 0 then
      begin
        j := Pred(i + k);
        Accepte(i, j,
          EstAllumee(passives, CCase[j])
          xor (j = APos.EnPassant)
        );
      end;
      { Prise côté roi. }
      if i mod 8 < 7 then
      begin
        j := Succ(i + k);
        Accepte(i, j,
          EstAllumee(passives, CCase[j])
          xor (j = APos.EnPassant)
        );
      end;
    end else
    { Tour. }
    if EstAllumee(APos.Tours, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          EstAllumee(CCibles[Tour, i], CCase[j])
          and (not EstAllumee(actives, CCase[j]))
          and ((CChemin[i, j] and toutes) = 0)
        );
    end else
    { Cavalier. }
    if EstAllumee(APos.Cavaliers, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          EstAllumee(CCibles[Cavalier, i], CCase[j])
          and not EstAllumee(actives, CCase[j])
        );
    end else
    { Fou. }
    if EstAllumee(APos.Fous, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          EstAllumee(CCibles[Fou, i], CCase[j])
          and (not EstAllumee(actives, CCase[j]))
          and ((CChemin[i, j] and toutes) = 0)
        );
    end else
    { Dame. }
    if EstAllumee(APos.Dames, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          EstAllumee(CCibles[Dame, i], CCase[j])
          and (not EstAllumee(actives, CCase[j]))
          and ((CChemin[i, j] and toutes) = 0)
        );
    end else
    { Roi. }
    if EstAllumee(APos.Rois, CCase[i]) then
    begin
      for j := A1 to H8 do
        Accepte(i, j,
          EstAllumee(CCibles[Roi, i], CCase[j])
          and not EstAllumee(actives, CCase[j])
        );
    end;
  end;
  ACompte := LCompte;
end;

function GenereCoups(const APos: TPosition): TDamier;
var
  LListe: array[0..0] of integer;
  LCompte: integer;
begin
  result := GenereCoups(APos, LListe, LCompte, TRUE);
end;

function GenereCoupsNombre(const APos: TPosition): integer;
var
  LListe: array[0..0] of integer;
begin
  GenereCoups(APos, LListe, result, TRUE);
end;

end.
