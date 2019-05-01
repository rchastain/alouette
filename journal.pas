
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
var GLog: text;
{$ENDIF}

class procedure TJournal.Ajoute(const aLine: string);
begin
{$IFDEF DEBUG}
  WriteLn(GLog, Concat(DateTimeToStr(Now()), ' ', aLine));
  Flush(GLog);
{$ENDIF}
end;

var
  LName: string;
  
initialization
  LName := FormatDateTime('yyyymmddhhnnss".log"', Now);
{$IFDEF DEBUG}
  Assign(GLog, LName);
  if FileExists(LName) then
    Append(GLog)
  else
    Rewrite(GLog);
{$ENDIF}

finalization
{$IFDEF DEBUG}
  Close(GLog);
{$ENDIF}

end.
