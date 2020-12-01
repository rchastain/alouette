
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
procedure MovePieceIdx(var AType, ASide: TBoard; const AFr, ATo: integer; const APreserve: boolean = FALSE);

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

procedure MovePieceIdx(var AType, ASide: TBoard; const AFr, ATo: integer; const APreserve: boolean);
begin
  Assert((AFr >= 0) and (AFr <= 63));
  Assert((ATo >= 0) and (ATo <= 63));
  AType := AType and not CIdxToSqr[AFr] or CIdxToSqr[ATo];
  if APreserve then
    ASide := ASide or CIdxToSqr[ATo]
  else
    ASide := ASide and not CIdxToSqr[AFr] or CIdxToSqr[ATo];
end;

end.
