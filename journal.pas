
{**
  @abstract(Journal.)
  Unité pour la production d'un journal.
}

unit Journal;

{$IFDEF DEBUG}
{$ASSERTIONS ON}
{$ENDIF}

interface

uses
  SysUtils;

type
  TJournal = class
    class procedure Ajoute(const ALigne: string; const AHtml: boolean = FALSE);
    class procedure AjouteTable(const ACoups, ANotes: array of integer; const n: integer; const ATitre: string);
  end;

implementation

uses
  Damier;

{$IFDEF DEBUG}
var
  GFichier: array[boolean] of text;
{$ENDIF}

class procedure TJournal.Ajoute(const ALigne: string; const AHtml: boolean);
begin
{$IFDEF DEBUG}
  if AHtml then
    WriteLn(GFichier[AHtml], ALigne)
  else
    WriteLn(GFichier[AHtml], Concat(DateTimeToStr(Now()), ' ', ALigne));
  Flush(GFichier[AHtml]);
{$ENDIF}
end;

class procedure TJournal.AjouteTable(const ACoups, ANotes: array of integer; const n: integer; const ATitre: string);
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
  WriteLn(GFichier[TRUE], s);
  Flush(GFichier[TRUE]);
{$ENDIF}
end;

const
  CLogFolderName = 'journal';
  
var
  LName: array[boolean] of string;
{$IFDEF DEBUG}
  LIdx: boolean;
{$ENDIF}

initialization
  LName[FALSE] := FormatDateTime('yyyymmddhhnnss".log"', Now);
  LName[TRUE] := ChangeFileExt(LName[FALSE], '.html');
{$IFDEF DEBUG}
  Assert(DirectoryExists(CLogFolderName) or CreateDir (CLogFolderName));

  for LIdx := FALSE to TRUE do
  begin
    Assign(GFichier[LIdx], CLogFolderName + DirectorySeparator + LName[LIdx]);
    Rewrite(GFichier[LIdx]);
  end;
  WriteLn(
    GFichier[TRUE],
    '<!DOCTYPE html>' + LineEnding +
    '<html>' + LineEnding +
    '<head>' + LineEnding +
    '<link rel="stylesheet" href="style.css">' + LineEnding +
    '</head>' + LineEnding +
    '<body>'
  );
{$ENDIF}

finalization
{$IFDEF DEBUG}
  WriteLn(
    GFichier[TRUE],
    '</body>' + LineEnding +
    '</html>'
  );
  Close(GFichier[FALSE]);
  Close(GFichier[TRUE]);
{$ENDIF}

end.
