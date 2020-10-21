
{**
  @abstract(Déclarations des tables.)
  Déclarations des tables préfabriquées. Fonctions utilisant les tables et remplaçant les fonctions correspondantes de l'unité Damier.
}

unit Tables;

interface

uses
  Board;

const  
  CTargets: array[TPieceType, A1..H8] of TBoard = ({$I targets});
  CPath: array[A1..H8, A1..H8] of TBoard = ({$I path});
  CIndexToSquare: array[A1..H8] of TBoard = ({$I index});
  CCoordToSquare: array[0..7, 0..7] of TBoard = ({$I coordinates});
  CColumn: array[0..7] of TBoard = ({$I column});

function IsOnIdx(const ABrd: TBoard; const AIdx: integer): boolean;
procedure SwitchOnIdx(var ABrd: TBoard; const AIdx: integer);
procedure SwitchOffIdx(var ABrd: TBoard; const AIdx: integer);
procedure MovePieceIdx(var AType, ACoul: TBoard; const ADep, AArr: integer; const ASuper: boolean = FALSE);
function CountSquaresOn(ABrd: TBoard): integer;

implementation

function IsOnIdx(const ABrd: TBoard; const AIdx: integer): boolean;
begin
  Assert(CIndexToSquare[AIdx] <> 0);
  result := (ABrd and CIndexToSquare[AIdx]) = CIndexToSquare[AIdx];
end;

procedure SwitchOnIdx(var ABrd: TBoard; const AIdx: integer);
begin
  ABrd := ABrd or CIndexToSquare[AIdx];
end;

procedure SwitchOffIdx(var ABrd: TBoard; const AIdx: integer);
begin
  ABrd := ABrd and not CIndexToSquare[AIdx];
end;

procedure MovePieceIdx(var AType, ACoul: TBoard; const ADep, AArr: integer; const ASuper: boolean);
begin
  Assert((ADep >= 0) and (ADep <= 63));
  Assert((AArr >= 0) and (AArr <= 63));
  
  AType := AType and not CIndexToSquare[ADep] or CIndexToSquare[AArr];
  
  if ASuper then
    ACoul := ACoul or CIndexToSquare[AArr]
  else
    ACoul := ACoul and not CIndexToSquare[ADep] or CIndexToSquare[AArr];
end;

function CountSquaresOn(ABrd: TBoard): integer;
begin
  ABrd := (ABrd and $5555555555555555) + ((ABrd shr  1) and $5555555555555555);
  ABrd := (ABrd and $3333333333333333) + ((ABrd shr  2) and $3333333333333333);
  ABrd := (ABrd and $0F0F0F0F0F0F0F0F) + ((ABrd shr  4) and $0F0F0F0F0F0F0F0F);
  ABrd := (ABrd and $00FF00FF00FF00FF) + ((ABrd shr  8) and $00FF00FF00FF00FF);
  ABrd := (ABrd and $0000FFFF0000FFFF) + ((ABrd shr 16) and $0000FFFF0000FFFF);
  ABrd := (ABrd and $00000000FFFFFFFF) + ((ABrd shr 32) and $00000000FFFFFFFF);
  result := integer(ABrd);
end;

end.
