
unit Meilleur;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean): string;

implementation

uses
  SysUtils, Interprete, Coups, Roque, Damier, Tables, Journal, Histoire;

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
    if EstAllumeeIndex(APos.Pions,     LInd) then Inc(result,    1 * LCoul) else
    if EstAllumeeIndex(APos.Cavaliers, LInd) then Inc(result,    3 * LCoul) else
    if EstAllumeeIndex(APos.Fous,      LInd) then Inc(result,    4 * LCoul) else
    if EstAllumeeIndex(APos.Tours,     LInd) then Inc(result,    7 * LCoul) else
    if EstAllumeeIndex(APos.Dames,     LInd) then Inc(result,   15 * LCoul) else
    if EstAllumeeIndex(APos.Rois,      LInd) then Inc(result, 1000 * LCoul);
  end;
  if APos.Trait then
    result := -1 * result;
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
    (*
    if (APieces and LPos.Rois) <> 0 then
      Inc(result);
    *)
    LPieces := APieces and LPos.Rois;      if LPieces <> 0 then Inc(result, 10000);
    LPieces := APieces and LPos.Dames;     if LPieces <> 0 then Inc(result,    15 * CompteCases(LPieces));
    LPieces := APieces and LPos.Tours;     if LPieces <> 0 then Inc(result,     7 * CompteCases(LPieces));
    LPieces := APieces and LPos.Fous;      if LPieces <> 0 then Inc(result,     4 * CompteCases(LPieces));
    LPieces := APieces and LPos.Cavaliers; if LPieces <> 0 then Inc(result,     3 * CompteCases(LPieces));
    LPieces := APieces and LPos.Pions;     if LPieces <> 0 then Inc(result,     1 * CompteCases(LPieces));
  end;

var
  LActives, LPassives, LMenaces: TDamier;
  LBonusRoque, LMalusPiecesMenacees, LBonusNombreCoups, LBalanceMateriel: integer;
  
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
  LBalanceMateriel := Materiel(LPos);
  
  result :=
    0
    + LBalanceMateriel
    + LBonusRoque
    + LBonusNombreCoups div 10
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
  n, i: integer;
procedure Informe;
var
  i: integer;
  s: string;
begin
{$ifdef DEBUG}
  s := '';
  for i := 0 to Pred(n) do
    s := Concat(s, NomCoup(LListe[i]), CSeparateur[i < Pred(n)]);
  WriteLn('info ', s);
{$endif}
end;
begin
  result := 'a1a1';
  ChercheCoups(APos, LListe, n);
  ChercheRoque(APos, LListe, n);
  
  for i := 0 to Pred(n) do
    LEval[i] := Evalue(APos, LListe[i]);
  Trie(LListe, LEval, n);
  Informe;
  
  if EstUnRoque(APos, LListe[0]) and not AEchecs960 then
  begin
    Assert((LListe[0] div 100) mod 8 = CColE);
    Reformule(LListe[0]);
  end;
  
  result := NomCoup(LListe[0]);
  
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
