
{$asmMode intel}

uses
  SysUtils, Board, Tables;

type
  TBitCount = function(const ABrd: TBoard): integer;

function BC1(const ABrd: TBoard): integer;
var
  LIdx: integer;
begin
  result := 0;
  for LIdx := A1 to H8 do
    if IsOnIdx(ABrd, LIdx) then
      Inc(result);
end;

function BC2(const ABrd: TBoard): integer;
const
  C0 = TBoard(0);
  C1 = TBoard(1);
var
  LBrd: TBoard;
begin
  result := 0;
  LBrd := ABrd;
  while LBrd <> C0 do
  begin
    if LBrd and C1 = C1 then
      Inc(result);
    LBrd := LBrd shr 1;
  end;
end;

{ https://www.developpez.net/forums/d2001819/autres-langages/assembleur/x86-32-bits-64-bits/reecriture-pascal-d-fonction-assembleur/ }

function BC3(const ABrd: TBoard): integer;
var
  X: TBoard;
begin
  X := ABrd;
  X := (X and $5555555555555555) + ((X shr  1) and $5555555555555555);
  X := (X and $3333333333333333) + ((X shr  2) and $3333333333333333);
  X := (X and $0F0F0F0F0F0F0F0F) + ((X shr  4) and $0F0F0F0F0F0F0F0F);
  X := (X and $00FF00FF00FF00FF) + ((X shr  8) and $00FF00FF00FF00FF);
  X := (X and $0000FFFF0000FFFF) + ((X shr 16) and $0000FFFF0000FFFF);
  X := (X and $00000000FFFFFFFF) + ((X shr 32) and $00000000FFFFFFFF);
  result := integer(X);
end;

function BitCount3264(var n: Int64): integer; assembler; nostackframe; inline;
asm
  popcnt edx, dword ptr [n]
  popcnt eax, dword ptr [n+4]   
  add    eax, edx                   
end;

function BC4(const ABrd: TBoard): integer;
var
  LBrd: TBoard;
begin
  LBrd := ABrd;
  result := BitCount3264(LBrd);
end;

function BitCount64(n: Int64): integer; assembler; nostackframe; inline;
asm
  popcnt rax, rcx
end;

function BC5(const ABrd: TBoard): integer;
var
  LBrd: TBoard;
begin
  LBrd := ABrd;
  result := BitCount64(LBrd);
end;

procedure Test(ABrd: TBoard; AFunc: TBitCount);
var
  i, res: integer;
  t: cardinal;
begin
  t := GetTickCount64;
  for i := 1 to 1000000 do
    res := AFunc(ABrd);
  WriteLn(GetTickCount64 - t);
end;

const
  CBrd = %0100001000000000000000000000000000000000000000000000000001000010;

begin
  Test(CBrd, @BC1);
  Test(CBrd, @BC2);
  Test(CBrd, @BC3);
  Test(CBrd, @BC4);
  Test(CBrd, @BC5);
end.
