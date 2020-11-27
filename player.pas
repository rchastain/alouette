
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
function BestMove(const ATimeAv: integer; const ARandMove: boolean = FALSE): string;
function InstantMove: string;
procedure SetVariant(const AFrc: boolean);
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
  ResetHistory;
end;

procedure DoMove(const AMove: string);
begin
  if Move.DoMove(LPos, AMove) then
    History.AppendMove(AMove)
  else
    Log.Append(Format('** Impossible move: %s', [AMove]));
end;

function BestMove(const ATimeAv: integer; const ARandMove: boolean): string;
begin
  LMove := 'a1a1';
  result := GetBestMove(LPos, LVariant, ATimeAv, LMove, ARandMove);
end;

function InstantMove: string;
begin
  result := LMove;
end;

procedure SetVariant(const AFrc: boolean);
const
  CBoolToStr: array[boolean] of string = ('false', 'true');
begin
  Log.Append(Format('** SetVariant(%s)', [CBoolToStr[AFrc]]));
  LVariant := AFrc;
end;

function CurrentVariant: boolean;
begin
  result := LVariant;
end;

procedure SetPosition(const APos: string);
begin
  LPos := EncodePosition(APos, LVariant);
  ResetHistory;
end;

function CurrentPosition: TPosition;
begin
  result := LPos;
end;

initialization
  Reset;

end.
