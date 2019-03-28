
unit Utiles;

interface

function DecodeChaineCoup(const AChaine: string; out ADepart, AArrivee: integer): boolean;
function ChaineDateHeure: string;
function ChaineCompilateur: string;

implementation
  
function DecodeChaineCoup(const AChaine: string; out ADepart, AArrivee: integer): boolean;
const
  CIndisponible = -1;
begin
  result := (Length(AChaine) >= 4)
    and (AChaine[1] in ['a'..'h'])
    and (AChaine[2] in ['1'..'8'])
    and (AChaine[3] in ['a'..'h'])
    and (AChaine[4] in ['1'..'8']);
  if result then
  begin
    ADepart := 8 * (Ord(AChaine[2]) - Ord('1')) + (Ord(AChaine[1]) - Ord('a'));
    AArrivee := 8 * (Ord(AChaine[4]) - Ord('1')) + (Ord(AChaine[3]) - Ord('a'));
  end else
  begin
    ADepart := CIndisponible;
    AArrivee := CIndisponible;
  end;
end;

function ChaineDateHeure(const ADateFPC, AHeureFPC: string): string;
begin
  result := Concat(
    Copy(ADateFPC, 3, 2),
    Copy(ADateFPC, 6, 2),
    Copy(ADateFPC, 9, 2),
    Copy(AHeureFPC, 1, 2),
    Copy(AHeureFPC, 4, 2)
  );
end;

function ChaineDateHeure: string;
begin
  result := ChaineDateHeure(
    {$I %DATE%},
    {$I %TIME%}
  );
end;

function ChaineCompilateur: string;
begin
  result := 'FPC ' + {$I %FPCVERSION%};
end;

end.
