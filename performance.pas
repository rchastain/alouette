
{**
  @abstract(Génération chronométrée.)
  Génération chronométrée de tous les coups jusqu'à une profondeur donnée.
}

unit Performance;

interface

uses
  Echecs;
  
procedure EssaiPerf(const APosition: TPosition; const AProfondeur: integer = 5);

implementation

uses
{$IFDEF UNIX}
  horloge,
{$ENDIF}
  SysUtils, Deplacement, Coups, Roque, Damier, Tables, Journal, Tri;

function Evalue(const APos: TPosition; const ACoup: integer): integer;
var
  LPos: TPosition;
 {LPassives: ^TDamier;}
begin
  LPos := APos;
  result := Low(integer);
  if not FRejoue(LPos, NomCoup(ACoup)) then
    exit;
 {if LPos.Trait then
    LPassives := @LPos.Blanches
  else
    LPassives := @LPos.Noires;}
  result := 0 - Ord((FCoups(LPos) and {LPassives^}LPos.PiecesCouleur[not LPos.Trait] and LPos.Rois) <> 0);
end;

function NombreCoups(const APos: TPosition; const AProf: integer): int64;
var
  LListe, LEval: array[0..99] of integer;
  n, o, i: integer;
  LPos: TPosition;
begin
  result := 0;
  FCoups(APos, LListe, n);
  FRoque(APos, LListe, n);
  for i := 0 to Pred(n) do
    LEval[i] := Evalue(APos, LListe[i]);
  Trie(LListe, LEval, n);
  o := 0;
  while (o < n) and (LEval[o] = 0) do
    Inc(o);
  if AProf = 1 then
    result := o
  else
    for i := 0 to Pred(o) do
    begin
      LPos := APos;
      if not FRejoue(LPos, NomCoup(LListe[i])) then
      begin
        WriteLn('Il y a quelque chose de pourri dans ce programme.');
        continue;
      end;
      Inc(result, NombreCoups(LPos, Pred(AProf)));
    end;
end;

procedure EssaiPerf(const APosition: TPosition; const AProfondeur: integer);
var
  p: TPosition;
  i: integer;
  t: cardinal;
  n: int64;
begin
  p := APosition;
  WriteLn('Profondeur   Nombre trouvé   Temps écoulé');
  for i := 1 to AProfondeur do
  begin
    t := GetTickCount64;
    n := NombreCoups(p, i);
    t := GetTickCount64 - t;
    WriteLn(i:10, n:16, FormatDateTime('   hh:nn:ss:zzz', t / (1000 * SECSPERDAY)));
  end;
end;

end.
