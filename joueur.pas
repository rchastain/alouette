
{** @abstract(Joueur d'échecs artificiel.) }

unit Joueur;

interface

procedure Oublie;
procedure PositionDepart;
procedure Rejoue(const ACoup: string);
function Coup: string;
procedure ActiveMode960(const AValeur: boolean);
procedure NouvellePosition(const APos: string);

implementation

uses
  SysUtils, Journal, Echecs, Placement, Meilleur;
  
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
