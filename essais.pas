
{**
  @abstract(Essais.)
  Essais.
}

unit Essais;

interface

implementation

uses
  SysUtils, Classes, Echecs, Numero;
  
procedure EssaiEncodeDecode;
const
  CNomFich = 'chess960.fen';
var
  i: integer;
  p: TPosition;
begin
  Assert(FileExists(CNomFich), Format('Impossible de trouver le fichier %s.', [CNomFich]));
  with TStringList.Create do
  try
    LoadFromFile(CNomFich);
    Assert(Count = 960);
    for i := 0 to 959 do
    begin
      p := EncodePosition(strings[i], TRUE);
      Assert(Concat(DecodePosition(p, TRUE), ' 0 1') = strings[i], Format('Les deux chaînes ne sont pas identiques (%d).', [i]));
    end;
  finally
    Free;
  end;
end;

initialization
  EssaiEncodeDecode;
  
end.
