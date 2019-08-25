
unit Reglages;

interface

procedure LitFichierIni(out AVariante: boolean; out ARecherche: boolean);
procedure EcritFichierIni(const AVariante: boolean; const ARecherche: boolean);
function FichierIniExiste: boolean;

implementation

uses
  SysUtils, IniFiles;

const
  SECTION = '.';
  DEFAUT_VARIANTE = 'FALSE';
  DEFAUT_RECHERCHE = 'TRUE';
  CBoolStr: array[boolean] of string = ('false', 'true');
  
var
  LChemin: string;
  
procedure LitFichierIni(out AVariante: boolean; out ARecherche: boolean);
begin
  with TIniFile.Create(LChemin) do
  try
    AVariante := UpperCase(ReadString(SECTION, 'frc', DEFAUT_VARIANTE)) = 'TRUE';
    ARecherche := UpperCase(ReadString(SECTION, 'search', DEFAUT_RECHERCHE)) = 'TRUE';
  finally
    Free;
  end;
end;

procedure EcritFichierIni(const AVariante: boolean; const ARecherche: boolean);
begin
  with TIniFile.Create(LChemin) do
  try
    WriteString(SECTION, 'frc', CBoolStr[AVariante]);
    WriteString(SECTION, 'search', CBoolStr[ARecherche]);
    UpdateFile;
  finally
    Free;
  end;
end;

function FichierIniExiste: boolean;
begin
  result := FileExists(LChemin);
end;

begin
  LChemin := ChangeFileExt(ParamStr(0), '.ini');
end.
