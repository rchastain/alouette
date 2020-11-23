
{**
  @abstract(Génération des coups.)
  Génération des coups.
}

unit Moves;

interface

uses
  Chess, Board, Move;

function GenMoves(const APos: TPosition; var AList: array of integer; out ACount: integer; const AQuick: boolean = FALSE): TBoard; overload;
{** Renvoie un damier représentant les cases pouvant être atteintes. Les coups ne sont pas conservés. }
function GenMoves(const APos: TPosition): TBoard; overload;
function GetMovesCount(const APos: TPosition): integer;
function GenPotentialPawnMoves(const APos: TPosition): TBoard;
function IsCheck(const APos: TPosition): boolean;
function GetProtectionsCount(const APos: TPosition): integer;
function GetAttacksCount(const APos: TPosition): integer;

implementation

uses
  SysUtils, Tables, Log;

function GenMoves(const APos: TPosition; var AList: array of integer; out ACount: integer; const AQuick: boolean): TBoard;
var
  LCompte: integer = 0;

  procedure SaveMove(const i, j: integer; const p: TPieceType; const c: TMoveType = mtCommon);
  begin
    SwitchOn(result, CIdxToSqr[j]);
    Inc(LCompte);
    if not AQuick then
      if LCompte <= Length(AList) then
        AList[Pred(LCompte)] := EncodeMove(i, j, p, c)
      else
        Log.Append('** Cannot append move');
  end;

const
  CPion: array[boolean] of TPieceType = (ptWhitePawn, ptBlackPawn);
var
  { Toutes les pièces. }
  LPieces: TBoard;
  i, j, k: integer;
  LTargets, LActive, LPassive: TBoard;
begin
  LPieces := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  LActive := APos.Pieces[APos.Side];
  LPassive := APos.Pieces[not APos.Side];
  result := 0; { Damier vide. }
  i := BsfQWord(LActive);
  while LActive <> 0 do
  begin
    { Pion. }
    if IsOn(APos.Pawns, CIdxToSqr[i]) then
    begin
      k := 8 - 16 * Ord(APos.Side);
      j := i + k;
      if not IsOn(LPieces, CIdxToSqr[j]) then
      begin
        SaveMove(i, j, CPion[APos.Side]);
        if j div 8 = 3 * Ord(APos.Side) + 2 then
        begin
          j := j + k;
          if not IsOn(LPieces, CIdxToSqr[j]) then
            SaveMove(i, j, CPion[APos.Side]);
        end;
      end;
      if i mod 8 > 0 then
      begin
        j := Pred(i + k);
        if j = APos.EnPassant then
          SaveMove(i, j, CPion[APos.Side], mtEnPassant)
        else
          if IsOn(LPassive, CIdxToSqr[j]) then
            SaveMove(i, j, CPion[APos.Side], mtCapture);
      end;
      if i mod 8 < 7 then
      begin
        j := Succ(i + k);
        if j = APos.EnPassant then
          SaveMove(i, j, CPion[APos.Side], mtEnPassant)
        else
          if IsOn(LPassive, CIdxToSqr[j]) then
            SaveMove(i, j, CPion[APos.Side], mtCapture);
      end;
    end else
    { Tour. }
    if IsOn(APos.Rooks, CIdxToSqr[i]) then
    begin
      LTargets := CTargets[ptRook, i] and not APos.Pieces[APos.Side];
      j := BsfQWord(LTargets);
      while LTargets <> 0 do
      begin
        if (CPath[i, j] and LPieces) = 0 then
          if IsOn(LPassive, CIdxToSqr[j]) then
            SaveMove(i, j, ptRook, mtCapture)
          else
            SaveMove(i, j, ptRook);
        LTargets := LTargets and not CIdxToSqr[j];
        j := BsfQWord(LTargets);
      end;
    end else
    { Cavalier. }
    if IsOn(APos.Knights, CIdxToSqr[i]) then
    begin
      LTargets := CTargets[ptKnight, i] and not APos.Pieces[APos.Side];
      j := BsfQWord(LTargets);
      while LTargets <> 0 do
      begin
        if IsOn(LPassive, CIdxToSqr[j]) then
          SaveMove(i, j, ptKnight, mtCapture)
        else
          SaveMove(i, j, ptKnight);
        LTargets := LTargets and not CIdxToSqr[j];
        j := BsfQWord(LTargets);
      end;
    end else
    { Fou. }
    if IsOn(APos.Bishops, CIdxToSqr[i]) then
    begin
      LTargets := CTargets[ptBishop, i] and not APos.Pieces[APos.Side];
      j := BsfQWord(LTargets);
      while LTargets <> 0 do
      begin
        if (CPath[i, j] and LPieces) = 0 then
          if IsOn(LPassive, CIdxToSqr[j]) then
            SaveMove(i, j, ptBishop, mtCapture)
          else
            SaveMove(i, j, ptBishop);
        LTargets := LTargets and not CIdxToSqr[j];
        j := BsfQWord(LTargets);
      end;
    end else
    { Dame. }
    if IsOn(APos.Queens, CIdxToSqr[i]) then
    begin
      LTargets := CTargets[ptQueen, i] and not APos.Pieces[APos.Side];
      j := BsfQWord(LTargets);
      while LTargets <> 0 do
      begin
        if (CPath[i, j] and LPieces) = 0 then
          if IsOn(LPassive, CIdxToSqr[j]) then
            SaveMove(i, j, ptQueen, mtCapture)
          else
            SaveMove(i, j, ptQueen);
        LTargets := LTargets and not CIdxToSqr[j];
        j := BsfQWord(LTargets);
      end;
    end else
    { Roi. }
    if IsOn(APos.Kings, CIdxToSqr[i]) then
    begin
      LTargets := CTargets[ptKing, i] and not APos.Pieces[APos.Side];
      j := BsfQWord(LTargets);
      while LTargets <> 0 do
      begin
        if IsOn(LPassive, CIdxToSqr[j]) then
          SaveMove(i, j, ptKing, mtCapture)
        else
          SaveMove(i, j, ptKing);
        LTargets := LTargets and not CIdxToSqr[j];
        j := BsfQWord(LTargets);
      end;
    end;
    LActive := LActive and not CIdxToSqr[i];
    i := BsfQWord(LActive);
  end;
  ACount := LCompte;
end;

function GenMoves(const APos: TPosition): TBoard;
var
  LListe: array[0..0] of integer;
  LCompte: integer;
begin
  result := GenMoves(APos, LListe, LCompte, TRUE);
end;

function GetMovesCount(const APos: TPosition): integer;
var
  LListe: array[0..0] of integer;
begin
  GenMoves(APos, LListe, result, TRUE);
end;

function GenPotentialPawnMoves(const APos: TPosition): TBoard;
var
  { Pièces. }
  i, j: integer;
  LPion: TPieceType;
begin
  if APos.Side then
    LPion := ptBlackPawn
  else
    LPion := ptWhitePawn;
  result := 0; { Damier vide. }
  for i := A1 to H8 do if IsOn(APos.Pieces[APos.Side], CIdxToSqr[i]) then
  begin
    { Pion. }
    if IsOn(APos.Pawns, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[LPion, i], CIdxToSqr[j])
        and not IsOn(APos.Pieces[APos.Side], CIdxToSqr[j]) then
          SwitchOn(result, CIdxToSqr[j]);
    end;
  end;
end;

function IsCheck(const APos: TPosition): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  LPos.Side := not LPos.Side;
  result := (GenMoves(LPos) and LPos.Kings) <> 0;
end;

function GetProtectionsCount(const APos: TPosition): integer;
var
  { Toutes les pièces. }
  LPieces: TBoard;
  i, j, k: integer;
begin
  LPieces := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  result := 0;
  
  for i := A1 to H8 do if IsOn(APos.Pieces[APos.Side], CIdxToSqr[i]) then
  begin
    { Pion. }
    if IsOn(APos.Pawns, CIdxToSqr[i]) then
    begin
      k := 8 - 16 * Ord(APos.Side);
      j := i + k;
      { Prise côté A. }
      if i mod 8 > 3 then
      begin
        j := Pred(i + k);
        if IsOn(APos.Pieces[APos.Side] and APos.Pawns, CIdxToSqr[j]) then
          Inc(result);
      end;
      { Prise côté H. }
      if i mod 8 < 4 then
      begin
        j := Succ(i + k);
        if IsOn(APos.Pieces[APos.Side] and APos.Pawns, CIdxToSqr[j]) then
          Inc(result);
      end;
    end else
    { Tour. }
    if IsOn(APos.Rooks, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptRook, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[APos.Side] and (APos.Pawns or APos.Knights or APos.Bishops or APos.Rooks), CIdxToSqr[j])
        and ((CPath[i, j] and LPieces) = 0) then
          Inc(result);
    end else
    { Cavalier. }
    if IsOn(APos.Knights, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptKnight, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[APos.Side] and (APos.Pawns or APos.Knights or APos.Bishops), CIdxToSqr[j]) then
          Inc(result);
    end else
    { Fou. }
    if IsOn(APos.Bishops, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptBishop, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[APos.Side] and (APos.Pawns or APos.Knights or APos.Bishops), CIdxToSqr[j])
        and ((CPath[i, j] and LPieces) = 0) then
          Inc(result);
    end else
    { Dame. }
    if IsOn(APos.Queens, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptQueen, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[APos.Side] and (APos.Bishops or APos.Rooks or APos.Queens), CIdxToSqr[j])
        and ((CPath[i, j] and LPieces) = 0) then
          Inc(result);
    end else
    { Roi. }
    if IsOn(APos.Kings, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptKing, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[APos.Side] and APos.Pawns, CIdxToSqr[j]) then
          Inc(result);
    end;
  end;
end;

function GetAttacksCount(const APos: TPosition): integer;
var
  { Toutes les pièces. }
  LPieces: TBoard;
  i, j, k: integer;
begin
  LPieces := APos.Pieces[FALSE] or APos.Pieces[TRUE];
  result := 0;
  
  for i := A1 to H8 do if IsOn(APos.Pieces[APos.Side], CIdxToSqr[i]) then
  begin
    { Pion. }
    if IsOn(APos.Pawns, CIdxToSqr[i]) then
    begin
      k := 8 - 16 * Ord(APos.Side);
      j := i + k;
      { Prise côté A. }
      if i mod 8 > 3 then
      begin
        j := Pred(i + k);
        if IsOn(APos.Pieces[not APos.Side], CIdxToSqr[j]) then
          Inc(result);
      end;
      { Prise côté H. }
      if i mod 8 < 4 then
      begin
        j := Succ(i + k);
        if IsOn(APos.Pieces[not APos.Side], CIdxToSqr[j]) then
          Inc(result);
      end;
    end else
    { Tour. }
    if IsOn(APos.Rooks, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptRook, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[not APos.Side], CIdxToSqr[j])
        and ((CPath[i, j] and LPieces) = 0) then
          Inc(result);
    end else
    { Cavalier. }
    if IsOn(APos.Knights, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptKnight, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[not APos.Side], CIdxToSqr[j]) then
          Inc(result);
    end else
    { Fou. }
    if IsOn(APos.Bishops, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptBishop, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[not APos.Side], CIdxToSqr[j])
        and ((CPath[i, j] and LPieces) = 0) then
          Inc(result);
    end else
    { Dame. }
    if IsOn(APos.Queens, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptQueen, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[not APos.Side], CIdxToSqr[j])
        and ((CPath[i, j] and LPieces) = 0) then
          Inc(result);
    end else
    { Roi. }
    if IsOn(APos.Kings, CIdxToSqr[i]) then
    begin
      for j := A1 to H8 do
        if IsOn(CTargets[ptKing, i], CIdxToSqr[j])
        and IsOn(APos.Pieces[not APos.Side], CIdxToSqr[j]) then
          Inc(result);
    end;
  end;
end;

end.
