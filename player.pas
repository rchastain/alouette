
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
  LVariant: boolean;
  
procedure Reset;
begin
  //Log.Append(Format('%s %s %s', [{$I %FILE%}, {$I %LINE%}, 'Reset']));
  LPos := CZeroPosition;
end;

procedure LoadStartPosition;
begin
  //Log.Append(Format('%s %s %s', [{$I %FILE%}, {$I %LINE%}, 'LoadStartPosition']));
  LPos := EncodePosition;
  NewHistory;
end;

procedure DoMove(const AMove: string);
begin
  //Log.Append(Format('%s %s %s(%s)', [{$I %FILE%}, {$I %LINE%}, 'DoMove', AMove]));
  if TryDoMove(LPos, AMove) then
    History.AppendMove(AMove)
  else
    Log.Append(Format('Impossible de jouer %s.', [AMove]));
end;

function BestMove(const ATimeAvailable: integer): string;
begin
  //Log.Append(Format('%s %s %s(%d)', [{$I %FILE%}, {$I %LINE%}, 'BestMove', ATimeAvailable]));
  result := GetBestMove(LPos, LVariant, ATimeAvailable);
end;

function InstantMove: string;
begin
  //Log.Append(Format('%s %s %s', [{$I %FILE%}, {$I %LINE%}, 'InstantMove']));
  result := LTempMove;
end;

procedure SetVariant(const AValue: boolean);
begin
  Log.Append(Format('%s %s %s(%d)', [{$I %FILE%}, {$I %LINE%}, 'SetVariant', Ord(AValue)]));
  LVariant := AValue;
end;

function CurrentVariant: boolean;
begin
  result := LVariant;
end;

procedure SetPosition(const APos: string);
begin
  //Log.Append(Format('%s %s %s(%s)', [{$I %FILE%}, {$I %LINE%}, 'SetPosition', APos]));
  LPos := EncodePosition(APos, LVariant);
  NewHistory;
end;

function CurrentPosition: TPosition;
begin
  result := LPos;
end;

initialization
  Reset;
  LVariant := FALSE;

end.
