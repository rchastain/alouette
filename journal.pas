
{**
  @abstract(Journal.)
  Unité pour la production d'un journal.
}

unit Journal;

interface

uses
  SysUtils;

type
  TJournal = class
    class procedure Ajoute(const aLine: string);
  end;

implementation

{$IFDEF DEBUG}
var vLog: text;
{$ENDIF}

class procedure TJournal.Ajoute(const aLine: string);
begin
{$IFDEF DEBUG}
  WriteLn(vLog, Concat(DateTimeToStr(Now()), ' ', aLine));
  Flush(vLog);
{$ENDIF}
end;

var
  vName: string;
  
initialization
  //vName := ChangeFileExt(ParamStr(0), '.log');
  vName := FormatDateTime('yyyymmddhhnnss".log"', Now);
{$IFDEF DEBUG}
  Assign(vLog, vName);
  if FileExists(vName) then
    Append(vLog)
  else
    Rewrite(vLog);
{$ENDIF}

finalization
{$IFDEF DEBUG}
  Close(vLog);
{$ENDIF}

end.
