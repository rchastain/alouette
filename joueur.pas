
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
procedure RegleVariante(const AValeur: boolean);
function VarianteCourante: boolean;
procedure NouvellePosition(const APos: string);
function PositionCourante: TPosition;

implementation

uses
  SysUtils, Journal, Deplacement, Histoire,
{$IFDEF A}MeilleurA{$ENDIF}
{$IFDEF B}MeilleurB{$ENDIF}
{$IFDEF C}MeilleurC{$ENDIF}
  ;

var
  LPos: TPosition;
  LVarianteFRC: boolean;
  
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
    TJournal.Ajoute(Format('Impossible de jouer %s.', [ACoup]));
end;

function Coup(const ATempsDisponible: integer): string;
begin
  result := MeilleurCoup(LPos, LVarianteFRC, ATempsDisponible);
end;

procedure RegleVariante(const AValeur: boolean);
const
  CPrefixe: array[boolean] of string = ('dés', '');
begin
  TJournal.Ajoute(Format('Option échecs 960 %sactivée.', [CPrefixe[AValeur]]));
  LVarianteFRC := AValeur;
end;

function VarianteCourante: boolean;
begin
  result := LVarianteFRC;
end;

procedure NouvellePosition(const APos: string);
begin
  LPos := EncodePosition(APos, LVarianteFRC);
  NouvelleHistoire;
end;

function PositionCourante: TPosition;
begin
  result := LPos;
end;

initialization
  Oublie;
  LVarianteFRC := FALSE;
  
finalization

end.
