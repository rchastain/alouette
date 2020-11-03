
{**
  @abstract(Tables de damiers binaires précalculés.)
  Tables de damiers binaires précalculés.
  Cette unité inclut le code source produit par les programmes du dossier _factory_. 
  L'unité contient également des fonctions utilisant les tables et remplaçant les fonctions correspondantes de l'unité _board_.
  Ces dernières ont servi à fabriquer le code source, et ne sont pas utilisées dans le programme.
}

unit Tables;

interface

uses
  Board;

const  
  CTargets:  array[TPieceType, A1..H8] of TBoard = ({$I inc/targets.pas});
  CPath:     array[A1..H8, A1..H8]     of TBoard = ({$I inc/path.pas});
  CIdxToSqr: array[A1..H8]             of TBoard = ({$I inc/index.pas});
  CCrdToSqr: array[0..7, 0..7]         of TBoard = ({$I inc/coordinates.pas});
  CCol:      array[0..7]               of TBoard = ({$I inc/column.pas});

function IsOnIdx(const ABrd: TBoard; const AIdx: integer): boolean;
procedure SwitchOnIdx(var ABrd: TBoard; const AIdx: integer);
procedure SwitchOffIdx(var ABrd: TBoard; const AIdx: integer);
procedure MovePieceIdx(var AType, ASide: TBoard; const AFrom, ATo: integer; const APreserve: boolean = FALSE);
function BitCount(ABrd: TBoard): integer;

implementation

function IsOnIdx(const ABrd: TBoard; const AIdx: integer): boolean;
begin
  Assert(CIdxToSqr[AIdx] <> 0);
  result := (ABrd and CIdxToSqr[AIdx]) = CIdxToSqr[AIdx];
end;

procedure SwitchOnIdx(var ABrd: TBoard; const AIdx: integer);
begin
  ABrd := ABrd or CIdxToSqr[AIdx];
end;

procedure SwitchOffIdx(var ABrd: TBoard; const AIdx: integer);
begin
  ABrd := ABrd and not CIdxToSqr[AIdx];
end;

procedure MovePieceIdx(var AType, ASide: TBoard; const AFrom, ATo: integer; const APreserve: boolean);
begin
  Assert((AFrom >= 0) and (AFrom <= 63));
  Assert((ATo   >= 0) and (ATo   <= 63));
  
  AType := AType and not CIdxToSqr[AFrom] or CIdxToSqr[ATo];
  
  if APreserve then
    ASide := ASide or CIdxToSqr[ATo]
  else
    ASide := ASide and not CIdxToSqr[AFrom] or CIdxToSqr[ATo];
end;

function BitCount(ABrd: TBoard): integer;
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
