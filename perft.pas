
{**
  @abstract(Essai de performance.)
  Génération chronométrée des coups légaux jusqu'à une profondeur donnée.
  Permet d'évaluer la performance du générateur de coups, et de détecter les éventuelles erreurs,
  en comparant les résultats obtenus avec ceux d'autres programmes.
}

unit Perft;

interface

uses
  Chess;

function Start(const APos: TPosition; const ADepth: integer = 5; const AOutput: boolean = TRUE): integer;

implementation

uses
  SysUtils, Move, Moves, Castling, Board, Tables;

function IsLegal(const APos: TPosition; const AMove: integer): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  result := FALSE;
  if DoMove(LPos, MoveToStr(AMove)) then
    with LPos do
      result := (
        GenMoves(LPos)
        and Pieces[not Side]
        and Kings
      ) = 0;
end;

function Start(const APos: TPosition; const ADepth: integer; const AOutput: boolean): integer;
var
  LResult: array of int64;

  function GetMovesCount(const APos2: TPosition; const ADepth2: integer): int64;
  var
    LList: array[0..99] of integer;
    LCount, LLegalCount, i: integer;
    LPos: TPosition;
    LMove: string;
    LFrom, LTo: integer;
    LPieceType: TPieceType;
    LMoveType: TMoveTypeSet;
  begin
    result := 0;
    
    GenMoves(APos2, LList, LCount);
    GenCastling(APos2, LList, LCount);
    LLegalCount := 0;
    for i := 0 to Pred(LCount) do
      if IsLegal(APos2, LList[i]) then
      begin
        DecodeMove(LList[i], LFrom, LTo, LPieceType, LMoveType);
        Inc(LLegalCount, 3 * Ord(mtPromotion in LMoveType) + 1);
      end;
    
    Inc(LResult[Pred(ADepth2)], LLegalCount);
    
    if ADepth2 = 1 then
      result := LLegalCount
    else
      for i := 0 to Pred(LLegalCount) do
      begin
        LPos := APos2;
        LMove := MoveToStr(LList[i]);
        if DoMove(LPos, LMove) then
          Inc(result, GetMovesCount(LPos, Pred(ADepth2)))
        else
        begin
          WriteLn('Unexpected error');
          Exit;
        end;
      end;
  end;

var
  i: integer;
  t: cardinal;
  s: string;
begin
  SetLength(LResult, ADepth);
  for i := 0 to Pred(ADepth) do
    LResult[i] := 0;
  
  t := GetTickCount64;
  GetMovesCount(APos, ADepth);
  t := GetTickCount64 - t;
  
  result := LResult[Pred(ADepth)];
  
  if AOutput then
  begin
    s := Format('Perft(%%%dd) = %%%dd', [Length(IntToStr(ADepth)), Length(IntToStr(LResult[0]))]);
    
    for i := Pred(ADepth) downto 0 do
      WriteLn(Format(s, [ADepth - i, LResult[i]]));
    
    WriteLn('Time elapsed: ', FormatDateTime('hh:nn:ss:zzz', t / (1000 * SECSPERDAY)));
  end;
  
  SetLength(LResult, 0);
end;

end.
