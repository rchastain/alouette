
{**
  @abstract(Joueur d'échecs artificiel.)
  Joueur d'échecs artificiel.
}

unit Joueur;

interface

uses
  Echecs;

procedure Oublie;
procedure PositionDepart;
procedure Rejoue(const ACoup: string);
function Coup: string;
procedure ActiveEchecs960(const AValeur: boolean);
procedure NouvellePosition(const APos: string);
function PositionCourante: TPosition;

implementation

uses
  SysUtils, Journal, Deplacement, Meilleur, Histoire;

var
  p: TPosition;
  LMode960: boolean;
  
procedure Oublie;
begin
  p := CPositionVierge;
end;

procedure PositionDepart;
begin
  Assert(not LMode960);
  p := EncodePosition();
  NouvelleHistoire;
end;

procedure Rejoue(const ACoup: string);
begin
  if Rejoue_(p, ACoup) then
    Histoire.Ajoute(ACoup)
  else
    TJournal.Ajoute(Format('Impossible de jouer %s.', [ACoup]));
end;

function Coup: string;
begin
  result := MeilleurCoup(p, LMode960);
end;

procedure ActiveEchecs960(const AValeur: boolean);
const
  CPrefixe: array[boolean] of string = ('dés', '');
begin
  TJournal.Ajoute(Format('Option échecs 960 %sactivée.', [CPrefixe[AValeur]]));
  LMode960 := AValeur;
end;

procedure NouvellePosition(const APos: string);
begin
  p := EncodePosition(APos, LMode960);
  NouvelleHistoire;
end;

function PositionCourante: TPosition;
begin
  result := p;
end;

initialization
  Oublie;
  LMode960 := FALSE;
  
finalization

end.
