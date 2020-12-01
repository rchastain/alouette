
{**
  @abstract(Journal.)
  Unité pour la production du journal.
}

unit Log;

interface

uses
  SysUtils, Board;

procedure Append(const AText: string; const ASecondFile: boolean = FALSE); overload;
procedure Append(const AMoves: array of TMove; const ACount: integer; const ASecondFile: boolean = TRUE); overload;
procedure Append(const AMoves: array of TMove; const AValues: array of integer; const ACount: integer; const ASecondFile: boolean = TRUE); overload;

implementation

const
  CDir = 'log';
  
var
  LFile: array[boolean] of text;
  LCreated: boolean = FALSE;
  
procedure OpenLog;
var
  LName: string;
  LIdx: boolean;
begin
  LName := CDir + DirectorySeparator + FormatDateTime('yyyymmddhhnnsszzz"-%d.log"', Now);
  if DirectoryExists(CDir) or CreateDir(CDir) then
  begin
    for LIdx := FALSE to TRUE do
    begin
      Assign(LFile[LIdx], Format(LName, [Ord(LIdx)]));
      Rewrite(LFile[LIdx]);
    end;
    LCreated := TRUE;
  end;
end;

procedure CloseLog;
begin
  if LCreated then
  begin
    Close(LFile[FALSE]);
    Close(LFile[TRUE]);
  end;
end;

procedure Append(const AText: string; const ASecondFile: boolean);
begin
{$IFDEF DEBUG}
  if LCreated then
  begin
    WriteLn(LFile[ASecondFile], AText);
    Flush(LFile[ASecondFile]);
  end;
{$ENDIF}
end;

procedure Append(const AMoves: array of TMove; const ACount: integer; const ASecondFile: boolean);
{$IFDEF DEBUG}
var
  i: integer;
begin
  if LCreated then
  begin
    for i := 0 to Pred(ACount) do
      Write(LFile[ASecondFile], Format('%6s', [MoveToStr(AMoves[i])]));
    WriteLn(LFile[ASecondFile]);
    Flush(LFile[ASecondFile]);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure Append(const AMoves: array of TMove; const AValues: array of integer; const ACount: integer; const ASecondFile: boolean);
{$IFDEF DEBUG}
var
  i: integer;
begin
  if LCreated then
  begin
    for i := 0 to Pred(ACount) do
      Write(LFile[ASecondFile], Format('%6s', [MoveToStr(AMoves[i])]));
    WriteLn(LFile[ASecondFile]);
    for i := 0 to Pred(ACount) do
      Write(LFile[ASecondFile], Format('%6d', [AValues[i]]));
    WriteLn(LFile[ASecondFile]);
    Flush(LFile[ASecondFile]);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

{$IFDEF DEBUG}
initialization
  OpenLog;
finalization
  CloseLog;
{$ENDIF}
end.
