
unit Reglages;

interface

procedure LitIni(out AVariante: boolean; out ARecursion: integer);
procedure EcritIni(const AVariante: boolean; const ARecursion: integer);
function FichierExiste: boolean;

implementation

uses
  SysUtils, IniFiles;

const
  SECTION = '.';
  DEFAUT_VARIANTE = 'FALSE';
  DEFAUT_RECURSION = 1;
  CBoolStr: array[boolean] of string = ('false', 'true');
  
var
  LChemin: string;
  
procedure LitIni(out AVariante: boolean; out ARecursion: integer);
begin
  with TIniFile.Create(LChemin) do
  try
    AVariante := UpperCase(ReadString(SECTION, 'variante', DEFAUT_VARIANTE)) = 'TRUE';
    ARecursion := ReadInteger(SECTION, 'recursion', DEFAUT_RECURSION)
  finally
    Free;
  end;
end;

procedure EcritIni(const AVariante: boolean; const ARecursion: integer);
begin
  with TIniFile.Create(LChemin) do
  try
    WriteString(SECTION, 'variante', CBoolStr[AVariante]);
    WriteInteger(SECTION, 'recursion', ARecursion);
    UpdateFile;
  finally
    Free;
  end;
end;

function FichierExiste: boolean;
begin
  result := FileExists(LChemin);
end;

begin
  LChemin := ChangeFileExt(ParamStr(0), '.ini');
  (*
  LChemin := GetEnvironmentVariable('APPDATA');
  Assert(DirectoryExists(LChemin));
  LChemin := Concat(IncludeTrailingPathDelimiter(LChemin), ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  if not DirectoryExists(LChemin) then
    CreateDir(LChemin);
  Assert(DirectoryExists(LChemin));
  LChemin := Concat(IncludeTrailingPathDelimiter(LChemin), ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));
  *)
end.
