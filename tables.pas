
{**
@abstract(Déclarations des tables.)
Déclarations des tables fabriquées. Fonctions basées sur les tables, remplaçant les fonctions correspondantes de l'unité Damier.
}
unit Tables;

interface

uses
  Damier;

const  
  CCibles: array[TPiece, A1..H8] of TDamier = ({$I cibles.inc});
  CChemin: array[A1..H8, A1..H8] of TDamier = ({$I chemin.inc});
  CCase: array[A1..H8] of TDamier           = ({$I index.inc});
  CCaseXY: array[0..7, 0..7] of TDamier     = ({$I coord.inc});
  CColonne: array[0..7] of TDamier          = ({$I colonne.inc});

function EstAllumee_(const ADamier: TDamier; const AIndex: integer): boolean;
procedure Allume_(var ADamier: TDamier; const AIndex: integer);
procedure Eteint_(var ADamier: TDamier; const AIndex: integer);
procedure Deplace_(var AType, ACouleur: TDamier; const ADep, AArr: integer);
function CompteCases(const ADamier: TDamier): integer;

implementation

function EstAllumee_(const ADamier: TDamier; const AIndex: integer): boolean;
begin
  Assert(CCase[AIndex] <> 0);
  result := (ADamier and CCase[AIndex]) = CCase[AIndex];
end;

procedure Allume_(var ADamier: TDamier; const AIndex: integer);
begin
  ADamier := ADamier or CCase[AIndex];
end;

procedure Eteint_(var ADamier: TDamier; const AIndex: integer);
begin
  ADamier := ADamier and not CCase[AIndex];
end;

procedure Deplace_(var AType, ACouleur: TDamier; const ADep, AArr: integer);
begin
  Assert((ADep >= 0) and (ADep <= 63) and (AArr >= 0) and (AArr <= 63));
  AType := AType and not CCase[ADep] or CCase[AArr];
  ACouleur := ACouleur and not CCase[ADep] or CCase[AArr];
end;

function CompteCases(const ADamier: TDamier): integer;
var
  LIndex: integer;
begin
  result := 0;
  for LIndex := A1 to H8 do
    if EstAllumee_(ADamier, LIndex) then
      Inc(result);
end;

end.
