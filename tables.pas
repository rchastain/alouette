
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

function CompteCases(const ADamier: TDamier): integer;

implementation

procedure Allume(var ADamier: TDamier; const Ax, Ay: integer);
begin
  ADamier := ADamier or CCaseXY[Ax, Ay];
end;

function CompteCases(const ADamier: TDamier): integer;
var
  LIndex: integer;
begin
  result := 0;
  for LIndex := 0 to 63 do if Allumee(ADamier, CCase[LIndex]) then
    Inc(result);
end;

end.
