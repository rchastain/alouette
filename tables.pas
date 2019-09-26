
{**
  @abstract(Déclarations des tables.)
  Déclarations des tables préfabriquées. Fonctions utilisant les tables et remplaçant les fonctions correspondantes de l'unité Damier.
}

unit Tables;

interface

uses
  Damier;

const  
  CCibles:    array[TTypePiece, A1..H8] of TDamier = ({$I cibles.inc});
  CChemin:    array[A1..H8, A1..H8]     of TDamier = ({$I chemin.inc});
  CCaseIdx:   array[A1..H8]             of TDamier = ({$I index.inc});
  CCaseCoord: array[0..7, 0..7]         of TDamier = ({$I coordonnees.inc});
  CColonne:   array[0..7]               of TDamier = ({$I colonne.inc});

function EstAllumeeIdx(const ADam: TDamier; const AIdx: integer): boolean;
procedure AllumeIdx(var ADam: TDamier; const AIdx: integer);
procedure EteintIdx(var ADam: TDamier; const AIdx: integer);
procedure DeplaceIdx(var AType, ACoul: TDamier; const ADep, AArr: integer; const ASuper: boolean = FALSE);
function CompteCasesAllumees(X: TDamier): integer;

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

procedure DeplaceIdx(var AType, ACoul: TDamier; const ADep, AArr: integer; const ASuper: boolean);
begin
  Assert((ADep >= 0) and (ADep <= 63));
  Assert((AArr >= 0) and (AArr <= 63));
  
  AType := AType and not CCaseIdx[ADep] or CCaseIdx[AArr];
  
  if ASuper then
    ACoul := ACoul or CCaseIdx[AArr]
  else
    ACoul := ACoul and not CCaseIdx[ADep] or CCaseIdx[AArr];
end;

function CompteCasesAllumees(X: TDamier): integer;
begin
  X := (X and $5555555555555555) + ((X shr  1) and $5555555555555555);
  X := (X and $3333333333333333) + ((X shr  2) and $3333333333333333);
  X := (X and $0F0F0F0F0F0F0F0F) + ((X shr  4) and $0F0F0F0F0F0F0F0F);
  X := (X and $00FF00FF00FF00FF) + ((X shr  8) and $00FF00FF00FF00FF);
  X := (X and $0000FFFF0000FFFF) + ((X shr 16) and $0000FFFF0000FFFF);
  X := (X and $00000000FFFFFFFF) + ((X shr 32) and $00000000FFFFFFFF);
  result := integer(X);
end;

end.
