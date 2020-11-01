
{$ASSERTIONS ON}

uses
  SysUtils, Classes, Chess, Number;

const
  CFileName = 'fischerandom.fen';
  
var
  i: integer;
  LPos: TPosition;
  
begin
  Assert(FileExists(CFileName), Format('Impossible de trouver le fichier %s.', [CFileName]));
  with TStringList.Create do
  try
    LoadFromFile(CFileName);
    Assert(Count = 960);
    for i := 0 to 959 do
    begin
      LPos := EncodePosition(strings[i], TRUE);
      Assert(Concat(DecodePosition(LPos, TRUE), ' 0 1') = strings[i], Format('Les deux chaînes ne sont pas identiques (%d).', [i]));
    end;
  finally
    Free;
  end;
end.
