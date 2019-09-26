
unit Reglages;

interface

procedure LitFichierIni(out AVariante: boolean);
procedure EcritFichierIni(const AVariante: boolean);
function FichierIniExiste: boolean;

implementation

uses
  SysUtils, IniFiles;

const
  SECTION = '.';
  DEFAUT_VARIANTE = 'FALSE';
  CBoolStr: array[boolean] of string = ('false', 'true');
  
var
  LChemin: string;
  
procedure LitFichierIni(out AVariante: boolean);
begin
  with TIniFile.Create(LChemin) do
  try
    AVariante := UpperCase(ReadString(SECTION, 'frc', DEFAUT_VARIANTE)) = 'TRUE';
  finally
    Free;
  end;
end;

procedure EcritFichierIni(const AVariante: boolean);
begin
  with TIniFile.Create(LChemin) do
  try
    WriteString(SECTION, 'frc', CBoolStr[AVariante]);
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
