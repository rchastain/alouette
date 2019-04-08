
unit Meilleur;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean): string;

implementation

uses
  SysUtils, Deplacement, Coups, Roque, Damier, Tables, Journal, Histoire, TablesPieceCase;

function Position(const APos: TPosition): integer;
var
  LActives: TDamier;
  LInd, LIndInv: integer;
begin
  result := 0;
  
  with APos do
    if Trait then
      LActives := Noires
    else
      LActives := Blanches;
  
  for LInd := A1 to H8 do
    if EstAllumeeIndex(LActives, LInd) then
    begin
      if APos.Trait then
        LIndInv := 63 - LInd
      else
        LIndInv := LInd;
      if EstAllumeeIndex(APos.Pions,     LInd) then Inc(result, CTablePionBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Cavaliers, LInd) then Inc(result, CTableCavalierBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Fous,      LInd) then Inc(result, CTableFouBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Tours,     LInd) then Inc(result, CTableTourBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Dames,     LInd) then Inc(result, CTableDameBlanc[LIndInv]) else
      if EstAllumeeIndex(APos.Rois,      LInd) then Inc(result, CTableRoiBlanc[LIndInv]);
    end;
end;

function Materiel(const APos: TPosition): integer;
var
  LInd, LCoul: integer;
begin
  result := 0;
  for LInd := A1 to H8 do
  begin
    if EstAllumeeIndex(APos.Blanches, LInd) then
      LCoul := 1
    else if EstAllumeeIndex(APos.Noires, LInd) then
      LCoul := -1
    else
      continue;
    if EstAllumeeIndex(APos.Pions,     LInd) then Inc(result,   100 * LCoul) else
    if EstAllumeeIndex(APos.Cavaliers, LInd) then Inc(result,   320 * LCoul) else
    if EstAllumeeIndex(APos.Fous,      LInd) then Inc(result,   330 * LCoul) else
    if EstAllumeeIndex(APos.Tours,     LInd) then Inc(result,   500 * LCoul) else
    if EstAllumeeIndex(APos.Dames,     LInd) then Inc(result,   900 * LCoul) else
    if EstAllumeeIndex(APos.Rois,      LInd) then Inc(result, 20000 * LCoul);
  end;
  if APos.Trait then
    result := -1 * result;
end;

function Evalue2(const APos: TPosition; const ACoup: integer): integer;
var
  LPos1, LPos2, LPos3: TPosition;
  LListe1, LListe2: array[0..99] of integer;
  n, o, i, j, v, vmax: integer;
begin
  LPos1 := APos;
  result := Low(integer);
  if not Rejoue_(LPos1, NomCoup(ACoup)) then
    exit;
  ChercheCoups(LPos1, LListe1, n);
  result := High(integer);
  for i := 0 to Pred(n) do
  begin
    LPos2 := LPos1;
    if not Rejoue_(LPos2, NomCoup(LListe1[i])) then
      continue;
    
    if Materiel(LPos2) < 8 * 900 + 2 * 500 + 2 * 330 + 2 * 320 - 20000 then
      exit(Low(integer));
    
    ChercheCoups(LPos2, LListe2, o);
    vmax := Low(integer);
    for j := 0 to Pred(o) do
    begin
      LPos3 := LPos2;
      if not Rejoue_(LPos3, NomCoup(LListe2[j])) then
        continue;
      LPos3.Trait := not LPos3.Trait;
      v := Materiel(LPos3);
      if v > vmax then
        vmax := v;
    end;
    if vmax < result then
      result := vmax;
  end;
end;

function Inverse(const ACoup: string): string;
begin
  result := Concat(Copy(ACoup, 3, 2), Copy(ACoup, 1, 2));
end;

function Evalue(const APos: TPosition; const ACoup: integer): integer;
var
  LPos: TPosition;

  function Estime(const APieces: TDamier): integer;
  var
    LPieces: TDamier;
  begin
    result := 0;
    LPieces := APieces and LPos.Rois;      if LPieces <> 0 then Inc(result, 20000);
    LPieces := APieces and LPos.Dames;     if LPieces <> 0 then Inc(result,   900 * CompteCases(LPieces));
    LPieces := APieces and LPos.Tours;     if LPieces <> 0 then Inc(result,   500 * CompteCases(LPieces));
    LPieces := APieces and LPos.Fous;      if LPieces <> 0 then Inc(result,   330 * CompteCases(LPieces));
    LPieces := APieces and LPos.Cavaliers; if LPieces <> 0 then Inc(result,   320 * CompteCases(LPieces));
    LPieces := APieces and LPos.Pions;     if LPieces <> 0 then Inc(result,   100 * CompteCases(LPieces));
  end;

var
  LActives, LPassives, LMenaces: TDamier;
  LBonusRoque, LMalusPiecesMenacees, LBonusNombreCoups, LBalanceMateriel, LPosition: integer;
  LBonusMenacesActives: integer;
  
begin
  LPos := APos;
  LBonusRoque := 100 * Ord(EstUnRoque(LPos, ACoup));
  result := Low(integer);
  if not Rejoue_(LPos, NomCoup(ACoup)) then
    exit; 
  
  with LPos do
    if Trait then
    begin
      LActives := Noires;
      LPassives := Blanches;
    end else
    begin
      LActives := Blanches;
      LPassives := Noires;
    end;

  LMenaces := ChercheCoups(LPos);
  LMalusPiecesMenacees := Estime(LMenaces and LPassives);
  LPos.Trait := not LPos.Trait;
  LBonusNombreCoups := ChercheNombre(LPos);
  LPosition := Position(LPos);
  LBonusMenacesActives := Estime(ChercheCoups(LPos) and LActives);
  
  result :=
    0
    + LPosition
    + LBonusRoque
    + LBonusNombreCoups
    + LBonusMenacesActives
    - LMalusPiecesMenacees
    - Ord(NomCoup(ACoup) = Inverse(Dernier))
    - Ord(NomCoup(ACoup) = AvantDernier);
end;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean): string;
  procedure Trie(var ACoup, ANote: array of integer; const n: integer);
    procedure Echange(var ATab: array of integer; const i: integer);
    var
      j: integer;
    begin
      j := ATab[i];
      ATab[i] := ATab[i + 1];
      ATab[i + 1] := j;
    end;
  var
    LInd: integer;
    LFin: boolean;
  begin
    repeat
      LFin := TRUE;
      for LInd := 0 to n - 2 do
        if ANote[LInd] < ANote[LInd + 1] then
        begin
          Echange(ACoup, LInd);
          Echange(ANote, LInd);
          LFin := FALSE;
        end;
    until LFin;
  end;
const
  CSeparateur: array[boolean] of string = ('', ' ');
var
  LListe, LEval: array[0..99] of integer;
  n, i, coup: integer;
procedure Informe;
var
  i: integer;
  LChaine: string;
begin
  LChaine := '';
  for i := 0 to Pred(n) do
    LChaine := Format('%s%s(%d)%s', [LChaine, NomCoup(LListe[i]), LEval[i], CSeparateur[i < Pred(n)]]);
  TJournal.Ajoute(LChaine);
end;
begin
  result := '0000';
  ChercheCoups(APos, LListe, n);
  ChercheRoque(APos, LListe, n);
  for i := 0 to Pred(n) do
    LEval[i] := Evalue2(APos, LListe[i]);
  Trie(LListe, LEval, n);
{$IFDEF DEBUG}
  Informe;
{$ENDIF}
  n := 0;
  while LEval[n] = LEval[0] do
    Inc(n);
  for i := 0 to Pred(n) do
    LEval[i] := Evalue(APos, LListe[i]);
  Trie(LListe, LEval, n);
{$IFDEF DEBUG}
  Informe;
{$ENDIF}
  n := 0;
  while LEval[n] = LEval[0] do
    Inc(n);
  coup := LListe[Random(n)];
  if EstUnRoque(APos, coup) and not AEchecs960 then
  begin
    Assert((coup div 100) mod 8 = CColE);
    Reformule(coup);
  end;
  result := NomCoup(coup);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
