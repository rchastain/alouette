
{**
  @abstract(Génération des coups.)
  Génération des coups.
}

unit Coups;

interface

uses
  Echecs, Damier, Deplacement;

function FCoups(const APos: TPosition; var ALst: array of integer; out ACompte: integer; const ARapide: boolean = FALSE): TDamier; overload;
{** Renvoie un damier représentant les cases pouvant être atteintes. Les coups ne sont pas conservés. }
function FCoups(const APos: TPosition): TDamier; overload;
function FNombreCoups(const APos: TPosition): integer;
function FCoupsPotentielsPion(const APos: TPosition): TDamier;
function FEchec(const APos: TPosition): boolean;
function FProtections(const APos: TPosition): integer;

implementation

uses
  SysUtils, Tables;

function FCoups(const APos: TPosition; var ALst: array of integer; out ACompte: integer; const ARapide: boolean): TDamier;
var
  LCompte: integer = 0;
procedure Accepte(const i, j: integer; const p: TTypePiece; const c: TTypeCoup = tcOrdinaire);
begin
  Allume(result, CCaseIdx[j]);
  Inc(LCompte);
  if not ARapide then
  begin
    Assert(LCompte <= Length(ALst));
    ALst[Pred(LCompte)] := EncodeCoup(i, j, p, c);
  end;
end;
const
  CPion: array[boolean] of TTypePiece = (PionBlanc, PionNoir);
var
  { Pièces. }
  LToutes: TDamier;
  i, j, k: integer;
begin
  LToutes := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  
  result := 0; { Damier vide. }
  
  for i := A1 to H8 do if EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[i]) then
  begin
    { Pion. }
    if EstAllumee(APos.Pions, CCaseIdx[i]) then
    begin
      { Pas en avant. }
      k := 8 - 16 * Ord(APos.Trait);
      j := i + k;
      if not EstAllumee(LToutes, CCaseIdx[j]) then
      begin
        Accepte(i, j, CPion[APos.Trait]);
        { Second pas en avant. }
        if ((j div 8 = 2) and not APos.Trait)
        or ((j div 8 = 5) and APos.Trait) then
        begin
          j := j + k;
          if not EstAllumee(LToutes, CCaseIdx[j]) then
            Accepte(i, j, CPion[APos.Trait]);
        end;
      end;
      { Prise côté dame. }
      if i mod 8 > 0 then
      begin
        j := Pred(i + k);
        if EstAllumee(APos.Pieces[not APos.Trait], CCaseIdx[j]) xor (j = APos.EnPassant) then
          if j = APos.EnPassant then
          Accepte(i, j, CPion[APos.Trait], tcEnPassant) else
          Accepte(i, j, CPion[APos.Trait], tcCapture);
      end;
      { Prise côté roi. }
      if i mod 8 < 7 then
      begin
        j := Succ(i + k);
        if EstAllumee(APos.Pieces[not APos.Trait], CCaseIdx[j]) xor (j = APos.EnPassant) then
          if j = APos.EnPassant then
          Accepte(i, j, CPion[APos.Trait], tcEnPassant) else
          Accepte(i, j, CPion[APos.Trait], tcCapture);
      end;
    end else
    { Tour. }
    if EstAllumee(APos.Tours, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Tour, i], CCaseIdx[j])
        and not EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[j])
        and ((CChemin[i, j] and LToutes) = 0) then
          if EstAllumee(APos.Pieces[not APos.Trait], CCaseIdx[j]) then
          Accepte(i, j, Tour, tcCapture) else
          Accepte(i, j, Tour);
    end else
    { Cavalier. }
    if EstAllumee(APos.Cavaliers, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Cavalier, i], CCaseIdx[j])
        and not EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[j]) then
          if EstAllumee(APos.Pieces[not APos.Trait], CCaseIdx[j]) then
          Accepte(i, j, Cavalier, tcCapture) else
          Accepte(i, j, Cavalier);
    end else
    { Fou. }
    if EstAllumee(APos.Fous, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Fou, i], CCaseIdx[j])
        and not EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[j])
        and ((CChemin[i, j] and LToutes) = 0) then
          if EstAllumee(APos.Pieces[not APos.Trait], CCaseIdx[j]) then
          Accepte(i, j, Fou, tcCapture) else
          Accepte(i, j, Fou);
    end else
    { Dame. }
    if EstAllumee(APos.Dames, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Dame, i], CCaseIdx[j])
        and not EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[j])
        and ((CChemin[i, j] and LToutes) = 0) then
          if EstAllumee(APos.Pieces[not APos.Trait], CCaseIdx[j]) then
          Accepte(i, j, Dame, tcCapture) else
          Accepte(i, j, Dame);
    end else
    { Roi. }
    if EstAllumee(APos.Rois, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Roi, i], CCaseIdx[j])
        and not EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[j]) then
          if EstAllumee(APos.Pieces[not APos.Trait], CCaseIdx[j]) then
          Accepte(i, j, Roi, tcCapture) else
          Accepte(i, j, Roi);
    end;
  end;
  ACompte := LCompte;
end;

function FCoups(const APos: TPosition): TDamier;
var
  LListe: array[0..0] of integer;
  LCompte: integer;
begin
  result := FCoups(APos, LListe, LCompte, TRUE);
end;

function FNombreCoups(const APos: TPosition): integer;
var
  LListe: array[0..0] of integer;
begin
  FCoups(APos, LListe, result, TRUE);
end;

function FCoupsPotentielsPion(const APos: TPosition): TDamier;
procedure Accepte(const i, j: integer);
begin
  Allume(result, CCaseIdx[j]);
end;
var
  { Pièces. }
  i, j: integer;
  LPion: TTypePiece;
begin
  if APos.Trait then
    LPion := PionNoir
  else
    LPion := PionBlanc;
  result := 0; { Damier vide. }
  for i := A1 to H8 do if EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[i]) then
  begin
    { Pion. }
    if EstAllumee(APos.Pions, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[LPion, i], CCaseIdx[j])
        and not EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[j]) then
          Accepte(i, j);
    end;
  end;
end;

function FEchec(const APos: TPosition): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  LPos.Trait := not LPos.Trait;
  result := (FCoups(LPos) and LPos.Rois) <> 0;
end;

function FProtections(const APos: TPosition): integer;
(*
const
  CPion: array[boolean] of TTypePiece = (PionBlanc, PionNoir);
*)
var
  { Pièces. }
  LToutes: TDamier;
  i, j, k: integer;
begin
  LToutes := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  result := 0;
  
  for i := A1 to H8 do if EstAllumee(APos.Pieces[APos.Trait], CCaseIdx[i]) then
  begin
    { Pion. }
    if EstAllumee(APos.Pions, CCaseIdx[i]) then
    begin
      k := 8 - 16 * Ord(APos.Trait);
      j := i + k;
      { Prise côté A. }
      if i mod 8 > 0 then
      begin
        j := Pred(i + k);
        if EstAllumee(APos.Pieces[APos.Trait] and APos.Pions, CCaseIdx[j]) then
          Inc(result);
      end;
      { Prise côté H. }
      if i mod 8 < 7 then
      begin
        j := Succ(i + k);
        if EstAllumee(APos.Pieces[APos.Trait] and APos.Pions, CCaseIdx[j]) then
          Inc(result);
      end;
    end else
    { Tour. }
    if EstAllumee(APos.Tours, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Tour, i], CCaseIdx[j])
        and EstAllumee(APos.Pieces[APos.Trait] and (APos.Pions or APos.Cavaliers or APos.Fous or APos.Tours), CCaseIdx[j])
        and ((CChemin[i, j] and LToutes) = 0) then
          Inc(result);
    end else
    { Cavalier. }
    if EstAllumee(APos.Cavaliers, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Cavalier, i], CCaseIdx[j])
        and EstAllumee(APos.Pieces[APos.Trait] and (APos.Pions or APos.Cavaliers or APos.Fous), CCaseIdx[j]) then
          Inc(result);
    end else
    { Fou. }
    if EstAllumee(APos.Fous, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Fou, i], CCaseIdx[j])
        and EstAllumee(APos.Pieces[APos.Trait] and (APos.Pions or APos.Cavaliers or APos.Fous), CCaseIdx[j])
        and ((CChemin[i, j] and LToutes) = 0) then
          Inc(result);
    end else
    { Dame. }
    if EstAllumee(APos.Dames, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Dame, i], CCaseIdx[j])
        and EstAllumee(APos.Pieces[APos.Trait] and (APos.Pions or APos.Cavaliers or APos.Fous or APos.Tours or APos.Dames), CCaseIdx[j])
        and ((CChemin[i, j] and LToutes) = 0) then
          Inc(result);
    end else
    { Roi. }
    if EstAllumee(APos.Rois, CCaseIdx[i]) then
    begin
      for j := A1 to H8 do
        if EstAllumee(CCibles[Roi, i], CCaseIdx[j])
        and EstAllumee(APos.Pieces[APos.Trait] and APos.Pions, CCaseIdx[j]) then
          Inc(result);
    end;
  end;
end;

end.
