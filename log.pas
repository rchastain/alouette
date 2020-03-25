
{**
  @abstract(Journal.)
  Unité pour la production d'un journal.
}

unit Log;

interface

uses
{$IFDEF UNIX}
  CWString,
{$ENDIF}
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
  LName: array[boolean] of string;
  LIdx: boolean;
begin
  LName[FALSE] := FormatDateTime('yyyymmddhhnnss".log"', Now);
  LName[TRUE] := ChangeFileExt(LName[FALSE], '-bestmove.log');
  Assert(DirectoryExists(CNomDossier) or CreateDir(CNomDossier));
  for LIdx := FALSE to TRUE do
  begin
    Assign(LFichier[LIdx], CNomDossier + DirectorySeparator + LName[LIdx]);
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
{$IFDEF DEBUG}
  WriteLn(LFichier[ASecondFile], {$IFDEF UNIX}Utf8ToAnsi({$ENDIF}AText{$IFDEF UNIX}){$ENDIF});
  Flush(LFichier[ASecondFile]);
{$ENDIF}
end;

procedure Append(const AMoves: array of integer; const AMovesCount: integer; const ASecondFile: boolean);
{$IFDEF DEBUG}
var
  s: string;
  i: integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  s := '';
  for i := 0 to Pred(AMovesCount) do
    s := s + Format('%6s', [MoveToStr(AMoves[i])]);
  WriteLn(LFichier[ASecondFile], s);
  Flush(LFichier[ASecondFile]);
{$ENDIF}
end;

procedure Append(const AMoves, AValues: array of integer; const AMovesCount: integer; const ASecondFile: boolean);
{$IFDEF DEBUG}
var
  s: string;
  i: integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  s := '';
  for i := 0 to Pred(AMovesCount) do
    s := s + Format('%6s', [MoveToStr(AMoves[i])]);
  WriteLn(LFichier[ASecondFile], s);
  s := '';
  for i := 0 to Pred(AMovesCount) do
    s := s + Format('%6d', [AValues[i]]);
  WriteLn(LFichier[ASecondFile], s);
  Flush(LFichier[ASecondFile]);
{$ENDIF}
end;

{$IFDEF DEBUG}
initialization
  OpenLog;
finalization
  CloseLog;
{$ENDIF}

end.
