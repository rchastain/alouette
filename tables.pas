
{**
@abstract(Déclarations des tables.)
Déclarations des tables fabriquées. Fonctions basées sur les tables, remplaçant les fonctions correspondantes de l'unité Damier.
}
unit Tables;

interface

uses
  Damier;

const  
  CCibles:    array[TPiece, A1..H8] of TDamier = ({$I cibles.inc});
  CChemin:    array[A1..H8, A1..H8] of TDamier = ({$I chemin.inc});
  CCaseIndex: array[A1..H8]         of TDamier = ({$I index.inc});
  CCaseCoord: array[0..7, 0..7]     of TDamier = ({$I coord.inc});
  CColonne:   array[0..7]           of TDamier = ({$I colonne.inc});

function EstAllumeeIndex(const ADam: TDamier; const AInd: integer): boolean;
procedure AllumeIndex(var ADam: TDamier; const AInd: integer);
procedure EteintIndex(var ADam: TDamier; const AInd: integer);
procedure DeplaceIndex(var AType, ACoul: TDamier; const ADep, AArr: integer; const ASuper: boolean = FALSE);
function CompteCases(const ADam: TDamier): integer;

implementation

function EstAllumeeIndex(const ADam: TDamier; const AInd: integer): boolean;
begin
  Assert(CCaseIndex[AInd] <> 0);
  result := (ADam and CCaseIndex[AInd]) = CCaseIndex[AInd];
end;

procedure AllumeIndex(var ADam: TDamier; const AInd: integer);
begin
  ADam := ADam or CCaseIndex[AInd];
end;

procedure EteintIndex(var ADam: TDamier; const AInd: integer);
begin
  ADam := ADam and not CCaseIndex[AInd];
end;

procedure DeplaceIndex(var AType, ACoul: TDamier; const ADep, AArr: integer; const ASuper: boolean);
begin
  Assert((ADep >= 0) and (ADep <= 63));
  Assert((AArr >= 0) and (AArr <= 63));
  
  AType := AType and not CCaseIndex[ADep] or CCaseIndex[AArr];
  
  if ASuper then
    ACoul := ACoul or CCaseIndex[AArr]
  else
    ACoul := ACoul and not CCaseIndex[ADep] or CCaseIndex[AArr];
end;

function CompteCases(const ADam: TDamier): integer;
var
  LIndex: integer;
begin
  result := 0;
  for LIndex := A1 to H8 do
    if EstAllumeeIndex(ADam, LIndex) then
      Inc(result);
end;

end.
