
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
function Coup(const ATempsDisponible: integer): string;
function CoupImmediat: string;
procedure RegleVariante(const AValeur: boolean);
function VarianteCourante: boolean;
procedure NouvellePosition(const APos: string);
function PositionCourante: TPosition;

implementation

uses
  SysUtils, Journal, Deplacement, Histoire, Meilleur;

var
  LPos: TPosition;
  LEchecs960: boolean;
  
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
    Histoire.Ajoute(ACoup)
  else
    Journal.Ajoute(Format('Impossible de jouer %s.', [ACoup]));
end;

function Coup(const ATempsDisponible: integer): string;
begin
  result := MeilleurCoup(LPos, LEchecs960, ATempsDisponible);
end;

function CoupImmediat: string;
begin
  result := LCoupProv;
end;

procedure RegleVariante(const AValeur: boolean);
const
  CPrefixe: array[boolean] of string = ('dés', '');
begin
  Journal.Ajoute(Format('Option échecs 960 %sactivée.', [CPrefixe[AValeur]]));
  LEchecs960 := AValeur;
end;

function VarianteCourante: boolean;
begin
  result := LEchecs960;
end;

procedure NouvellePosition(const APos: string);
begin
  LPos := EncodePosition(APos, LEchecs960);
  NouvelleHistoire;
end;

function PositionCourante: TPosition;
begin
  result := LPos;
end;

initialization
  Oublie;
  LEchecs960 := FALSE;
  
finalization

end.
