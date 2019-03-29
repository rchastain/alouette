
{** @abstract(Joueur d'échecs artificiel.) }

unit Joueur;

interface

function DecodeChaineCoup(const AChaine: string; out ADepart, AArrivee: integer): boolean;
procedure Oublie;
procedure PositionDepart;
procedure Rejoue(const ACoup: string);
function Coup: string;
procedure ActiveMode960(const AValeur: boolean);
procedure NouvellePosition(const APos: string);

implementation

uses
  SysUtils, Journal, Echecs, Interprete, Meilleur;

function DecodeChaineCoup(const AChaine: string; out ADepart, AArrivee: integer): boolean;
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

var
  p: TPosition;
  c960: boolean;
  
procedure Oublie;
begin
  p := CPositionVierge;
end;

procedure PositionDepart;
begin
  Assert(not c960);
  p := EncodePosition();
end;

procedure Rejoue(const ACoup: string);
begin
  if not Rejoue_(p, ACoup) then
    TJournal.Ajoute(Format('Coup refusé "%s".', [ACoup]));
end;

function Coup: string;
begin
  result := MeilleurCoup(p);
end;

procedure ActiveMode960(const AValeur: boolean);
const
  CPrefixe: array[boolean] of string = ('dés', '');
begin
  TJournal.Ajoute(Format('Option échecs 960 %sactivée.', [CPrefixe[AValeur]]));
  c960 := AValeur;
end;

procedure NouvellePosition(const APos: string);
begin
  p := EncodePosition(APos, c960);
end;

initialization
  Oublie;
  c960 := FALSE;
  
finalization

end.
