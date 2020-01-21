
{**
  @abstract(Journal.)
  Unité pour la production d'un journal.
}

unit Journal;

interface

uses
{$IFDEF UNIX}
  CWString,
{$ENDIF}
  SysUtils;

procedure CommenceJournal;
procedure TermineJournal;
procedure Ajoute(const ALigne: string; const AHtml: boolean = FALSE);
procedure AjouteTable(const ACoups, ANotes: array of integer; const n: integer; const ATitre: string);

implementation

uses
  Damier;

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
  LName[TRUE] := ChangeFileExt(LName[FALSE], '.html');

  Assert(DirectoryExists(CNomDossier) or CreateDir(CNomDossier));

  for LIdx := FALSE to TRUE do
  begin
    Assign(LFichier[LIdx], CNomDossier + DirectorySeparator + LName[LIdx]);
    Rewrite(LFichier[LIdx]);
  end;
  WriteLn(
    LFichier[TRUE],
    '<!DOCTYPE html>' + LineEnding +
    '<html>' + LineEnding +
    '<head>' + LineEnding +
    '<link rel="stylesheet" href="style.css">' + LineEnding +
    '</head>' + LineEnding +
    '<body>'
    );
end;

procedure TermineJournal;
begin
  WriteLn(
    LFichier[TRUE],
    '</body>' + LineEnding +
    '</html>'
  );
  Close(LFichier[FALSE]);
  Close(LFichier[TRUE]);
end;

procedure Ajoute(const ALigne: string; const AHtml: boolean);
begin
{$IFDEF DEBUG}
  WriteLn(LFichier[AHtml], {$IFDEF UNIX}Utf8ToAnsi({$ENDIF}ALigne{$IFDEF UNIX}){$ENDIF});
  Flush(LFichier[AHtml]);
{$ENDIF}
end;

procedure AjouteTable(const ACoups, ANotes: array of integer; const n: integer; const ATitre: string);
{$IFDEF DEBUG}
var
  s: string;
  i: integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  s := '<table><caption>' + ATitre + '</caption>' + LineEnding;
  s := s + '<tr>';
  for i := 0 to Pred(n) do
    s := s + Format('<th>%s</th>', [NomCoup(ACoups[i])]);
  s := s + '</tr>';
  s := s + '<tr>';
  for i := 0 to Pred(n) do
    s := s + Format('<td>%d</td>', [ANotes[i]]);
  s := s + '</tr>';
  s := s + '</table>';
  WriteLn(LFichier[TRUE], s);
  Flush(LFichier[TRUE]);
{$ENDIF}
end;

{$IFDEF DEBUG}
initialization
  CommenceJournal;
finalization
  TermineJournal;
{$ENDIF}

end.
