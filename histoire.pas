
unit Histoire;

interface

procedure NouvelleHistoire;
procedure Ajoute(const ACoup: string);
function Dernier: string;
function AvantDernier: string;

implementation

uses
  SysUtils, Classes;
  
var
  LListe: TStringList;

procedure NouvelleHistoire;
begin
  LListe.Clear;
end;

procedure Ajoute(const ACoup: string);
begin
  LListe.Append(ACoup);
end;

function Dernier: string;
begin
  if LListe.Count < 2 then
    result := ''
  else
    result := LListe[LListe.Count - 2];
end;

function AvantDernier: string;
begin
  if LListe.Count < 4 then
    result := ''
  else
    result := LListe[LListe.Count - 4];
end;

initialization
  LListe := TStringList.Create;
  
finalization
  LListe.Free;
  
end.
