
{**
  @abstract(Déclarations des tables.)
  Déclarations des tables préfabriquées. Fonctions basées sur les tables, remplaçant les fonctions correspondantes de l'unité Damier.
}

unit Tables;

interface

uses
  Damier;

const  
  CCibles:    array[TPiece, A1..H8] of TDamier = ({$I cibles.inc});
  CChemin:    array[A1..H8, A1..H8] of TDamier = ({$I chemin.inc});
  CCaseIdx:   array[A1..H8]         of TDamier = ({$I index.inc});
  CCaseCoord: array[0..7, 0..7]     of TDamier = ({$I coordonnees.inc});
  CColonne:   array[0..7]           of TDamier = ({$I colonne.inc});

function EstAllumeeIdx(const ADam: TDamier; const AIdx: integer): boolean;
procedure AllumeIdx(var ADam: TDamier; const AIdx: integer);
procedure EteintIdx(var ADam: TDamier; const AIdx: integer);
procedure DeplaceIdx(var AType, ACoul: TDamier; const ADep, AArr: integer; const APiecesSuperposees: boolean = FALSE);
function CompteCasesAllumees(const ADam: TDamier): integer;

implementation

function EstAllumeeIdx(const ADam: TDamier; const AIdx: integer): boolean;
begin
  Assert(CCaseIdx[AIdx] <> 0);
  result := (ADam and CCaseIdx[AIdx]) = CCaseIdx[AIdx];
end;

procedure AllumeIdx(var ADam: TDamier; const AIdx: integer);
begin
  ADam := ADam or CCaseIdx[AIdx];
end;

procedure EteintIdx(var ADam: TDamier; const AIdx: integer);
begin
  ADam := ADam and not CCaseIdx[AIdx];
end;

procedure DeplaceIdx(var AType, ACoul: TDamier; const ADep, AArr: integer; const APiecesSuperposees: boolean);
begin
  Assert((ADep >= 0) and (ADep <= 63));
  Assert((AArr >= 0) and (AArr <= 63));
  
  AType := AType and not CCaseIdx[ADep] or CCaseIdx[AArr];
  
  if APiecesSuperposees then
    ACoul := ACoul or CCaseIdx[AArr]
  else
    ACoul := ACoul and not CCaseIdx[ADep] or CCaseIdx[AArr];
end;

function CompteCasesAllumees(const ADam: TDamier): integer;
var
  LIdx: integer;
begin
  result := 0;
  for LIdx := A1 to H8 do
    if EstAllumeeIdx(ADam, LIdx) then
      Inc(result);
end;

end.
