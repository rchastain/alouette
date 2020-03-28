
{$ASSERTIONS ON}

uses
  SysUtils, Classes, Chess, Number;

const
  CFileName = 'fischerandom.fen';
  
var
  i: integer;
  p: TPosition;
  
begin
  Assert(FileExists(CFileName), Format('Impossible de trouver le fichier %s.', [CFileName]));
  with TStringList.Create do
  try
    LoadFromFile(CFileName);
    Assert(Count = 960);
    for i := 0 to 959 do
    begin
      p := EncodePosition(strings[i], TRUE);
      Assert(Concat(DecodePosition(p, TRUE), ' 0 1') = strings[i], Format('Les deux chaînes ne sont pas identiques (%d).', [i]));
    end;
  finally
    Free;
  end;
end.
