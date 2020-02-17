
{**
  @abstract(Joueur d'échecs artificiel.)
  Joueur d'échecs artificiel.
}

unit Player;

interface

uses
  Chess;

procedure Oublie;
procedure PositionDepart;
procedure Rejoue(const ACoup: string);
function Coup(const ATempsDisponible: integer): string;
function CoupImmediat: string;
procedure RegleVariante(const AValeur: boolean);
function VarianteCourante: boolean;
procedure NouvellePosition(const APos: string);
function PositionCourante: TPosition;

implementation

uses
  SysUtils, Log, Move, History, BestMove;

var
  LPos: TPosition;
  L960: boolean;
  
procedure Oublie;
begin
  LPos := CPositionVierge;
end;

procedure PositionDepart;
begin
  LPos := EncodePosition();
  NouvelleHistoire;
end;

procedure Rejoue(const ACoup: string);
begin
  if FRejoue(LPos, ACoup) then
    History.Ajoute(ACoup)
  else
    Log.Ajoute(Format('Impossible de jouer %s.', [ACoup]));
end;

function Coup(const ATempsDisponible: integer): string;
begin
  result := GetBestMove(LPos, L960, ATempsDisponible);
end;

function CoupImmediat: string;
begin
  result := LTempMove;
end;

procedure RegleVariante(const AValeur: boolean);
const
  CPrefixe: array[boolean] of string = ('dés', '');
begin
  Log.Ajoute(Format('Option échecs 960 %sactivée.', [CPrefixe[AValeur]]));
  L960 := AValeur;
end;

function VarianteCourante: boolean;
begin
  result := L960;
end;

procedure NouvellePosition(const APos: string);
begin
  LPos := EncodePosition(APos, L960);
  NouvelleHistoire;
end;

function PositionCourante: TPosition;
begin
  result := LPos;
end;

initialization
  Oublie;
  L960 := FALSE;
  
finalization

end.
