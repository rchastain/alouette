
{**
  @abstract(Journal.)
  Unité pour la production du journal.
}

unit Log;

interface

uses
  SysUtils;

procedure Append(const AText: string; const ASecondFile: boolean = FALSE); overload;
procedure Append(const AMoves: array of integer; const AMovesCount: integer; const ASecondFile: boolean = TRUE); overload;
procedure Append(const AMoves, AValues: array of integer; const AMovesCount: integer; const ASecondFile: boolean = TRUE); overload;

implementation

uses
  Board;

const
  CNomDossier = 'log';
  
var
  LFichier: array[boolean] of text;

procedure OpenLog;
var
  LFileName: string;
  LIdx: boolean;
begin
  LFileName := CNomDossier + DirectorySeparator + FormatDateTime('yyyymmddhhnnsszzz"-%d.log"', Now);
  if not (DirectoryExists(CNomDossier) or CreateDir(CNomDossier)) then
    raise Exception.Create('Cannot create directory');
  for LIdx := FALSE to TRUE do
  begin
    Assign(LFichier[LIdx], Format(LFileName, [Ord(LIdx)]));
    Rewrite(LFichier[LIdx]);
  end;
end;

procedure CloseLog;
begin
  Close(LFichier[FALSE]);
  Close(LFichier[TRUE]);
end;

procedure Append(const AText: string; const ASecondFile: boolean);
begin
  WriteLn(LFichier[ASecondFile], AText);
  Flush(LFichier[ASecondFile]);
end;

procedure Append(const AMoves: array of integer; const AMovesCount: integer; const ASecondFile: boolean);
var
  s: string;
  i: integer;
begin
  s := '';
  for i := 0 to Pred(AMovesCount) do
    s := s + Format('%6s', [MoveToStr(AMoves[i])]);
  WriteLn(LFichier[ASecondFile], s);
  Flush(LFichier[ASecondFile]);
end;

procedure Append(const AMoves, AValues: array of integer; const AMovesCount: integer; const ASecondFile: boolean);
var
  s: string;
  i: integer;
begin
  s := '';
  for i := 0 to Pred(AMovesCount) do
    s := s + Format('%6s', [MoveToStr(AMoves[i])]);
  WriteLn(LFichier[ASecondFile], s);
  s := '';
  for i := 0 to Pred(AMovesCount) do
    s := s + Format('%6d', [AValues[i]]);
  WriteLn(LFichier[ASecondFile], s);
  Flush(LFichier[ASecondFile]);
end;

initialization
  OpenLog;
  
finalization
  CloseLog;

end.
