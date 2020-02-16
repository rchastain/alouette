
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

procedure CommenceJournal;
procedure TermineJournal;
procedure Ajoute(const ALigne: string; const ASecondFile: boolean = FALSE);
procedure AjouteTable(const ACoups: array of integer; const n: integer; const ASecondFile: boolean = TRUE); overload;
procedure AjouteTable(const ACoups, ANotes: array of integer; const n: integer; const ASecondFile: boolean = TRUE); overload;

implementation

uses
  Board;

const
  CNomDossier = 'journal';
  
var
  LFichier: array[boolean] of text;
  
procedure CommenceJournal;
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

procedure TermineJournal;
begin
  Close(LFichier[FALSE]);
  Close(LFichier[TRUE]);
end;

procedure Ajoute(const ALigne: string; const ASecondFile: boolean);
begin
{$IFDEF DEBUG}
  WriteLn(LFichier[ASecondFile], {$IFDEF UNIX}Utf8ToAnsi({$ENDIF}ALigne{$IFDEF UNIX}){$ENDIF});
  Flush(LFichier[ASecondFile]);
{$ENDIF}
end;

procedure AjouteTable(const ACoups: array of integer; const n: integer; const ASecondFile: boolean);
{$IFDEF DEBUG}
var
  s: string;
  i: integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  s := '';
  for i := 0 to Pred(n) do
    s := s + Format('%s,', [NomCoup(ACoups[i])]);
  WriteLn(LFichier[ASecondFile], s);
  Flush(LFichier[ASecondFile]);
{$ENDIF}
end;

procedure AjouteTable(const ACoups, ANotes: array of integer; const n: integer; const ASecondFile: boolean);
{$IFDEF DEBUG}
var
  s: string;
  i: integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  s := '';
  for i := 0 to Pred(n) do
    s := s + Format('%s,', [NomCoup(ACoups[i])]);
  WriteLn(LFichier[ASecondFile], s);
  s := '';
  for i := 0 to Pred(n) do
    s := s + Format('%d,', [ANotes[i]]);
  WriteLn(LFichier[ASecondFile], s);
  Flush(LFichier[ASecondFile]);
{$ENDIF}
end;

{$IFDEF DEBUG}
initialization
  CommenceJournal;
finalization
  TermineJournal;
{$ENDIF}

end.
