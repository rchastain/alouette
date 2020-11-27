
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

function Start(const AIniPos: TPosition; const ADepth: integer = 5; const AOutput: boolean = TRUE): integer;
procedure DisplayLegalMoves(const APos: TPosition);

implementation

uses
  SysUtils, Classes, Move, Moves, Castling, Board;

function IsLegal(const APos: TPosition; const AMove: TMove): boolean;
var
  LPos: TPosition;
begin
  LPos := APos;
  result := FALSE;
  if DoMove(LPos, AMove) then
    with LPos do
      result := (
        GenMoves(LPos)
        and Pieces[not Side]
        and Kings
      ) = 0;
end;

function Start(const AIniPos: TPosition; const ADepth: integer; const AOutput: boolean): integer;
var
  LRes: array of integer;

  procedure GetMovesCount(const APos: TPosition; const ARecurse: integer);
  var
    LList: array[0..199] of TMove;
    LCount, LLegalCount, i: integer;
    LPos: TPosition;
  begin
    result := 0;
    Initialize(LList);
    GenMoves(APos, LList, LCount);
    GenCastling(APos, LList, LCount);
    LLegalCount := 0;
    for i := 0 to Pred(LCount) do
      if IsLegal(APos, LList[i]) then
        Inc(LLegalCount);
    Inc(LRes[Pred(ADepth - ARecurse)], LLegalCount);
    
    if ARecurse > 0 then
      for i := 0 to Pred(LLegalCount) do
      begin
        LPos := APos;
        if DoMove(LPos, LList[i]) then
          GetMovesCount(LPos, Pred(ARecurse))
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
  SetLength(LRes, ADepth);
  for i := 0 to Pred(ADepth) do
    LRes[i] := 0;
  
  t := GetTickCount64;
  GetMovesCount(AIniPos, Pred(ADepth));
  t := GetTickCount64 - t;
  
  result := LRes[Pred(ADepth)];
  
  if AOutput then
  begin
    s := Format('Perft(%%%dd) = %%%dd', [Length(IntToStr(ADepth)), Length(IntToStr(LRes[Pred(ADepth)]))]);
    for i := 0 to Pred(ADepth) do
      WriteLn(Format(s, [Succ(i), LRes[i]]));
    WriteLn('Time elapsed: ', FormatDateTime('hh:nn:ss:zzz', t / (1000 * SECSPERDAY)));
  end;
  
  SetLength(LRes, 0);
end;

procedure DisplayLegalMoves(const APos: TPosition);
var
  LList: array[0..199] of TMove;
  LCount, i: integer;
  LMoves: TStringList;
  LSqr, LPrevSqr: string;
begin
  Initialize(LList);
  LMoves := TStringList.Create;
  LMoves.Sorted := TRUE;
  
  GenMoves(APos, LList, LCount);
  GenCastling(APos, LList, LCount);
  
  for i := 0 to Pred(LCount) do
    if IsLegal(APos, LList[i]) then
      LMoves.Append(MoveToStr(LList[i]));
  
  WriteLn(Format('%d legal moves', [LMoves.Count]));
  
  if LMoves.Count > 0 then
  begin
    LPrevSqr := Copy(LMoves[0], 1, 2);
    for i := 0 to Pred(LMoves.Count) do
    begin
      LSqr := Copy(LMoves[i], 1, 2);
      if LSqr = LPrevSqr then
        Write(Format('%-6s', [LMoves[i]]))
      else
      begin
        LPrevSqr := LSqr;
        Write(LineEnding, Format('%-6s', [LMoves[i]]));
      end;
    end;
    WriteLn;
  end;
  
  LMoves.Free;
end;

end.
