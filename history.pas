
{**
  @abstract(Histoire des coups joués.)
  Histoire des coups joués.
}

unit History;

interface

procedure NewHistory;
procedure AppendMove(const AMove: string);
function LatestMove: string;
function PreviousMove: string;

implementation

uses
  SysUtils, Classes;

var
  LList: TStringList;

procedure NewHistory;
begin
  LList.Clear;
end;

procedure AppendMove(const AMove: string);
begin
  LList.Append(AMove);
end;

function LatestMove: string;
begin
  if LList.Count < 2 then
    result := ''
  else
    result := LList[LList.Count - 2];
end;

function PreviousMove: string;
begin
  if LList.Count < 4 then
    result := ''
  else
    result := LList[LList.Count - 4];
end;

initialization
  LList := TStringList.Create;
  
finalization
  LList.Free;
  
end.
