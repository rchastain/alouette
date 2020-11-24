
{**
  @abstract(Histoire des coups joués.)
  Histoire des coups joués.
}

unit History;

interface

procedure ResetHistory;
procedure AppendMove(const AMove: string);

implementation

uses
  SysUtils, Classes;

var
  LList: TStringList;

procedure ResetHistory;
begin
  LList.Clear;
end;

procedure AppendMove(const AMove: string);
begin
  LList.Append(AMove);
end;

initialization
  LList := TStringList.Create;
  
finalization
  LList.Free;
  
end.
