
unit Meilleur;

interface

uses
  Echecs, Interprete, Coups, Roque, Damier, Tables, Journal;

function MeilleurCoup(const APos: TPosition): string;

implementation

uses
  SysUtils;

function Evalue(const APos: TPosition; ACoup: integer): integer;
var
  p: TPosition;
  actives, passives, toutes, menaces: TDamier;

  function Estime(const APiecesMenacees: TDamier): integer;
  begin
    result := 0;
    (*
    if (APiecesMenacees and p.Rois) <> CDamierVide then
      Inc(result);
    *)
    if (APiecesMenacees and p.Rois) <> CDamierVide then
      Inc(result, 1000) else
    if (APiecesMenacees and p.Dames) <> CDamierVide then
      Inc(result, 10 * CompteCases(APiecesMenacees and p.Dames)) else
    if (APiecesMenacees and p.Tours) <> CDamierVide then
      Inc(result, 5 * CompteCases(APiecesMenacees and p.Tours)) else
    if (APiecesMenacees and p.Fous) <> CDamierVide then
      Inc(result, 3 * CompteCases(APiecesMenacees and p.Fous)) else
    if (APiecesMenacees and p.Cavaliers) <> CDamierVide then
      Inc(result, 2 * CompteCases(APiecesMenacees and p.Cavaliers));
  end;

begin
  result := 0;
  p := APos;
  Rejoue_(p, NomCoup(ACoup)); 
  with p do
  begin
    if Trait then
    begin
      actives := Noires;
      passives := Blanches;
    end else
    begin
      actives := Blanches;
      passives := Noires;
    end;
    toutes := Blanches or Noires;
  end;
  menaces := GenereCoups(p);
  result := -1 * Estime(menaces and passives);
end;

function MeilleurCoup(const APos: TPosition): string;
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
  CVirguleSi: array[boolean] of string = ('', ', ');
var
  liste, eval: array[0..99] of integer;
  n, i: integer;
  info: string;
begin
  result := 'a1a1';
  GenereCoups(APos, liste, n);
  GenereRoque(APos, liste, n);
  
  for i := 0 to Pred(n) do
    eval[i] := Evalue(APos, liste[i]);
  Trie(liste, eval, n);
  
  info := '';
  for i := 0 to Pred(n) do
    info := Concat(info, NomCoup(liste[i]), Format('{%d}', [eval[i]]), CVirguleSi[i < Pred(n)]);
  TJournal.Ajoute(info);
  
  result := NomCoup(liste[0]);
  if EstUnePromotion(APos, result) then
    result := Concat(result, 'q');
end;

end.
