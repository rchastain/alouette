
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
  GPos: TPosition;
  GMode960: boolean;
  
procedure Oublie;
begin
  GPos := CPositionVierge;
end;

procedure PositionDepart;
begin
  Assert(not GMode960);
  GPos := EncodePosition();
  NouvelleHistoire;
end;

procedure Rejoue(const ACoup: string);
begin
  if FRejoue(GPos, ACoup) then
    Histoire.Ajoute(ACoup)
  else
    TJournal.Ajoute(Format('Impossible de jouer %s.', [ACoup]));
end;

function Coup: string;
begin
  result := MeilleurCoup(GPos, GMode960);
end;

procedure ActiveEchecs960(const AValeur: boolean);
const
  CPrefixe: array[boolean] of string = ('dés', '');
begin
  TJournal.Ajoute(Format('Option échecs 960 %sactivée.', [CPrefixe[AValeur]]));
  GMode960 := AValeur;
end;

procedure NouvellePosition(const APos: string);
begin
  GPos := EncodePosition(APos, GMode960);
  NouvelleHistoire;
end;

function PositionCourante: TPosition;
begin
  result := GPos;
end;

initialization
  Oublie;
  GMode960 := FALSE;
  
finalization

end.
