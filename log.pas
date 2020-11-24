
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
  CNomDossier = 'log';
  
var
  LFichier: array[boolean] of text;
  LLogCreated: boolean = FALSE;
  
procedure OpenLog;
var
  LFileName: string;
  LIdx: boolean;
begin
  LFileName := CNomDossier + DirectorySeparator + FormatDateTime('yyyymmddhhnnsszzz"-%d.log"', Now);
  if DirectoryExists(CNomDossier) or CreateDir(CNomDossier) then
  begin
    for LIdx := FALSE to TRUE do
    begin
      Assign(LFichier[LIdx], Format(LFileName, [Ord(LIdx)]));
      Rewrite(LFichier[LIdx]);
    end;
    LLogCreated := TRUE;
  end;
end;

procedure CloseLog;
begin
  if LLogCreated then
  begin
    Close(LFichier[FALSE]);
    Close(LFichier[TRUE]);
  end;
end;

procedure Append(const AText: string; const ASecondFile: boolean);
begin
{$IFDEF DEBUG}
  if LLogCreated then
  begin
    WriteLn(LFichier[ASecondFile], AText);
    Flush(LFichier[ASecondFile]);
  end;
{$ENDIF}
end;

procedure Append(const AMoves: array of TMove; const ACount: integer; const ASecondFile: boolean);
{$IFDEF DEBUG}
var
  i: integer;
begin
  if LLogCreated then
  begin
    WriteLn(LFichier[ASecondFile], StringOfChar('=', 6 * ACount));
    for i := 0 to Pred(ACount) do
      Write(LFichier[ASecondFile], Format('%6s', [MoveToStr(AMoves[i])]));
    WriteLn(LFichier[ASecondFile]);
    WriteLn(LFichier[ASecondFile], StringOfChar('=', 6 * ACount));
    Flush(LFichier[ASecondFile]);
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
  if LLogCreated then
  begin
    WriteLn(LFichier[ASecondFile], StringOfChar('=', 6 * ACount));
    for i := 0 to Pred(ACount) do
      Write(LFichier[ASecondFile], Format('%6s', [MoveToStr(AMoves[i])]));
    WriteLn(LFichier[ASecondFile]);
    for i := 0 to Pred(ACount) do
      Write(LFichier[ASecondFile], Format('%6d', [AValues[i]]));
    WriteLn(LFichier[ASecondFile]);
    WriteLn(LFichier[ASecondFile], StringOfChar('=', 6 * ACount));
    Flush(LFichier[ASecondFile]);
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
