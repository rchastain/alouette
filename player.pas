
{**
  @abstract(Joueur d'échecs artificiel.)
  Joueur d'échecs artificiel.
}

unit Player;

interface

uses
  Chess;

procedure Reset;
procedure LoadStartPosition;
procedure DoMove(const AMove: string);
function BestMove(const ATimeAvailable: integer): string;
function InstantMove: string;
procedure SetVariant(const AValue: boolean);
function CurrentVariant: boolean;
procedure SetPosition(const APos: string);
function CurrentPosition: TPosition;

implementation

uses
  SysUtils, Log, Move, History, Best;

var
  LPos: TPosition;
  LVariant: boolean = FALSE;
  LMove: string;
  
procedure Reset;
begin
  LPos := CNewPos;
end;

procedure LoadStartPosition;
begin
  LPos := EncodePosition;
  NewHistory;
end;

procedure DoMove(const AMove: string);
begin
  if Move.DoMove(LPos, AMove) then
    History.AppendMove(AMove)
  else
    Log.Append(Format('** Impossible move: %s', [AMove]));
end;

function BestMove(const ATimeAvailable: integer): string;
begin
  LMove := 'a1a1';
  result := GetBestMove(LPos, LVariant, ATimeAvailable, LMove);
end;

function InstantMove: string;
begin
  result := LMove;
end;

procedure SetVariant(const AValue: boolean);
const
  CBoolToStr: array[boolean] of string = ('false', 'true');
begin
  Log.Append(Format('** SetVariant(%s)', [CBoolToStr[AValue]]));
  LVariant := AValue;
end;

function CurrentVariant: boolean;
begin
  result := LVariant;
end;

procedure SetPosition(const APos: string);
begin
  LPos := EncodePosition(APos, LVariant);
  NewHistory;
end;

function CurrentPosition: TPosition;
begin
  result := LPos;
end;

initialization
  Reset;

end.
