
unit Meilleur;

interface

uses
  Echecs;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean): string;

implementation

uses
  SysUtils, Interprete, Coups, Roque, Damier, Tables, Journal, Histoire;

function Inverse(const ACoup: string): string;
begin
  result := Concat(Copy(ACoup, 3, 2), Copy(ACoup, 1, 2));
end;

function Evalue(const APos: TPosition; ACoup: integer): integer;
var
  p: TPosition;

  function Estime(const APieces: TDamier): integer;
  var
    LPieces: TDamier;
  begin
    result := 0;
    (*
    if (APieces and p.Rois) <> 0 then
      Inc(result);
    *)
    LPieces := APieces and p.Rois;
    if LPieces <> 0 then Inc(result, 10000);
    LPieces := APieces and p.Dames;
    if LPieces <> 0 then Inc(result, 100 * CompteCases(LPieces));
    LPieces := APieces and p.Tours;
    if LPieces <> 0 then Inc(result,  50 * CompteCases(LPieces));
    LPieces := APieces and p.Fous;
    if LPieces <> 0 then Inc(result,  30 * CompteCases(LPieces));
    LPieces := APieces and p.Cavaliers;
    if LPieces <> 0 then Inc(result,  25 * CompteCases(LPieces));
    LPieces := APieces and p.Pions;
    if LPieces <> 0 then Inc(result,  10 * CompteCases(LPieces));
  end;

var
  actives, passives, menaces: TDamier;
  bonusRoque, malusPiecesMenacees, bonusNombreCoups: integer;
  
begin
  p := APos;
  bonusRoque := 100 * Ord(EstUnRoque(p, ACoup));
  result := Low(integer);
  if not Rejoue_(p, NomCoup(ACoup)) then
    exit; 
  
  with p do
    if Trait then
    begin
      actives := Noires;
      passives := Blanches;
    end else
    begin
      actives := Blanches;
      passives := Noires;
    end;
  
  menaces := ChercheCoups(p);
  malusPiecesMenacees := Estime(menaces and passives);
  p.Trait := not p.Trait;
  bonusNombreCoups := ChercheNombre(p);
  result :=
    0
    + bonusRoque
    + bonusNombreCoups div 3
    - malusPiecesMenacees
    //- Ord(NomCoup(ACoup) = Inverse(Dernier));
    - Ord(NomCoup(ACoup) = AvantDernier);
end;

function MeilleurCoup(const APos: TPosition; const AEchecs960: boolean): string;
  procedure Trie(var a, b: array of integer; const n: integer);
    procedure Echange(var x: array of integer; const i: integer);
    var
      j: integer;
    begin
      j := x[i];
      x[i] := x[i + 1];
      x[i + 1] := j;
    end;
  var
    i: integer;
    fin: boolean;
  begin
    repeat
      fin := TRUE;
      for i := 0 to n - 2 do
        if b[i] < b[i + 1] then
        begin
          Echange(a, i);
          Echange(b, i);
          fin := FALSE;
        end;
    until fin;
  end;
const
  CSeparateur: array[boolean] of string = ('', ' ');
var
  liste, eval: array[0..99] of integer;
  n, i: integer;
  info: string;
begin
  result := 'a1a1';
  ChercheCoups(APos, liste, n);
  GenereRoque(APos, liste, n);
  
  for i := 0 to Pred(n) do
    eval[i] := Evalue(APos, liste[i]);
  Trie(liste, eval, n);
{$IFDEF DEBUG}
  info := '';
  for i := 0 to Pred(n) do
    info := Concat(info, NomCoup(liste[i]), Format('{%d}', [eval[i]]), CSeparateur[i < Pred(n)]);
  WriteLn('info ', info);
{$ENDIF}
  if EstUnRoque(APos, liste[0]) and not AEchecs960 then
  begin
    Assert((liste[0] div 100) mod 8 = CColE);
    Reformule(liste[0]);
  end;
  
  result := NomCoup(liste[0]);
  
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
