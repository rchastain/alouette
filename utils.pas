
unit Utils;

interface

function BeginsWith(const ASubStr, AStr: string): boolean;
function WordsNumber(const AStr: string): integer;
function WordPresent(const AWord, AStr: string): boolean;
function GetWord(const AIndex: integer; const AStr: string): string;
function GetFen(const AStr: string): string;
function FixFen(const AStr: string; out AOld: string): string;
function IsChessMove(const AStr: string): boolean;
function IsGoCmd(const AStr: string; out MTime: integer): boolean; overload; // go movetime 500
function IsGoCmd(const AStr: string; out WTime, BTime: integer): boolean; overload; // go wtime 600000 btime 600000
function IsGoCmd(const AStr: string; out WTime, BTime, MTG: integer): boolean; overload; // go wtime 59559 btime 56064 movestogo 38
function IsGoCmd(const AStr: string; out WTime, BTime, WInc, BInc: integer): boolean; overload; // go wtime 60000 btime 60000 winc 1000 binc 1000
function IsPerftCmd(const AStr: string; out ADepth: integer): boolean;
function IsUsualStartPos(const AStr: string): boolean;

implementation

uses
  SysUtils, StrUtils, Chess;

function BeginsWith(const ASubStr, AStr: string): boolean;
begin
  result := Pos(ASubStr, AStr) = 1;
end;

function WordsNumber(const AStr: string): integer;
begin
  result := WordCount(AStr, [' ']);
end;

function WordPresent(const AWord, AStr: string): boolean;
begin
  result := IsWordPresent(AWord, AStr, [' ']);
end;

function GetWord(const AIndex: integer; const AStr: string): string;
begin
  result := ExtractWord(AIndex, AStr, [' ']);
end;

function GetFen(const AStr: string): string;
// position fen qbbnnrkr/pppppppp/8/8/8/8/PPPPPPPP/QBBNNRKR w HFhf - 0 1 moves e2e4
begin
  result := Format('%s %s %s %s %s %s', [
    GetWord(3, AStr),
    GetWord(4, AStr),
    GetWord(5, AStr),
    GetWord(6, AStr),
    GetWord(7, AStr),
    GetWord(8, AStr)
  ]);
end;

function FixFen(const AStr: string; out AOld: string): string;
// qbbnnrkr/pppppppp/8/8/8/8/PPPPPPPP/QBBNNRKR w HFhf - 0 1
begin
  result := Format('%s %s %s %s %s %s', [
    GetWord(1, AStr),
    GetWord(2, AStr),
    'KQkq',
    GetWord(4, AStr),
    GetWord(5, AStr),
    GetWord(6, AStr)
  ]);
  AOld := GetWord(3, AStr);
end;

function IsChessMove(const AStr: string): boolean;
begin
  result := (Length(AStr) >= 4)
    and (AStr[1] in ['a'..'h'])
    and (AStr[2] in ['1'..'8'])
    and (AStr[3] in ['a'..'h'])
    and (AStr[4] in ['1'..'8']);
end;

function IsGoCmd(const AStr: string; out MTime: integer): boolean;
// go movetime 500
begin
  MTime := 0;
  result := WordPresent('movetime',  AStr) and TryStrToInt(GetWord(3, AStr), MTime);
end;

function IsGoCmd(const AStr: string; out WTime, BTime: integer): boolean;
// go wtime 600000 btime 600000
begin
  WTime := 0;
  BTime := 0;
  result :=
    (GetWord(2, AStr) = 'wtime') and TryStrToInt(GetWord(3, AStr), WTime) and
    (GetWord(4, AStr) = 'btime') and TryStrToInt(GetWord(5, AStr), BTime);
end;

function IsGoCmd(const AStr: string; out WTime, BTime, MTG: integer): boolean;
// go wtime 59559 btime 56064 movestogo 38
begin
  WTime := 0;
  BTime := 0;
  MTG := 0;
  result :=
    (GetWord(2, AStr) = 'wtime')     and TryStrToInt(GetWord(3, AStr), WTime) and
    (GetWord(4, AStr) = 'btime')     and TryStrToInt(GetWord(5, AStr), BTime) and
    (GetWord(6, AStr) = 'movestogo') and TryStrToInt(GetWord(7, AStr), MTG)
end;

function IsGoCmd(const AStr: string; out WTime, BTime, WInc, BInc: integer): boolean;
// go wtime 60000 btime 60000 winc 1000 binc 1000
// go wtime 2039 winc 20 btime 1690 binc 20
begin
  WTime := 0;
  BTime := 0;
  WInc := 0;
  BInc := 0;
  result :=
    (GetWord(2, AStr) = 'wtime') and TryStrToInt(GetWord(3, AStr), WTime) and
    (GetWord(4, AStr) = 'btime') and TryStrToInt(GetWord(5, AStr), BTime) and
    (GetWord(6, AStr) = 'winc')  and TryStrToInt(GetWord(7, AStr), WInc) and
    (GetWord(8, AStr) = 'binc')  and TryStrToInt(GetWord(9, AStr), BInc) or   
    (GetWord(2, AStr) = 'wtime') and TryStrToInt(GetWord(3, AStr), WTime) and
    (GetWord(6, AStr) = 'btime') and TryStrToInt(GetWord(7, AStr), BTime) and
    (GetWord(4, AStr) = 'winc')  and TryStrToInt(GetWord(5, AStr), WInc) and
    (GetWord(8, AStr) = 'binc')  and TryStrToInt(GetWord(9, AStr), BInc);
end;

function IsPerftCmd(const AStr: string; out ADepth: integer): boolean;
var
  i: integer;
begin
  ADepth := 0;
  result := WordPresent('perft', AStr) and TryStrToInt(GetWord(2, AStr), i);
  if result then
    ADepth := i;
end;

function IsUsualStartPos(const AStr: string): boolean;
begin
  result :=
    (GetWord(1, AStr) = GetWord(1, CStartPos)) and
    (GetWord(2, AStr) = GetWord(2, CStartPos));
end;

end.
