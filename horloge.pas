
unit Horloge;

interface

uses
  BaseUnix, Linux;
  
function GetTickCount64: QWord;

implementation

function GetTickCount64: QWord;
var
  t: TTimeVal;
begin
  fpgettimeofday(@t, nil);
  result := Int64(t.tv_sec) * 1000 + t.tv_usec div 1000;
end;

end.
