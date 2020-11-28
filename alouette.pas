
{**
  @abstract(Programme principal du moteur d'échecs.)
  Interface UCI du moteur d'échecs.
}

program Alouette;

uses
{$IFDEF UNIX}
  CWString, CThreads,
{$ENDIF}
  Classes, SysUtils, Math, Log, Chess, Move, Best, Utils, Perft, Trees;

const
  CAppName    = 'Alouette';
  CAppVersion = '0.1.4';
  CAppAuthor  = 'Roland Chastain';
  
procedure Send(const AMessage: string; const AFlush: boolean = TRUE);
var
  LList: TStringList;
  i: integer;
begin
  WriteLn(output, AMessage);
  if AFlush then
    Flush(output);
  
  if Pos(LineEnding, AMessage) = 0 then
    Log.Append(Concat('<- ', AMessage))
  else
  begin
    LList := TStringList.Create;
    LList.Text := AMessage;
    for i := 0 to Pred(LList.Count) do
      Log.Append(Concat('<- ', LList[i]));
    LList.Free;
  end;
end;

type
  {** Fil d'exécution pour la recherche du meilleur coup. }
  TSearchThread = class(TThread)
    protected
      procedure Execute; override;
  end;

var
  LPos: TPosition;
  LFrc: boolean;
  LMove: string;
  LTimeAv: cardinal;
  LRandMove: boolean;
  
procedure TSearchThread.Execute;
var
  LTime: cardinal;
begin
  LTime := GetTickCount64;
  LMove := GetBestMove(LPos, LFrc, LTimeAv, LMove, LRandMove);
  LTime := GetTickCount64 - LTime;
  if not Terminated then
  begin
    Send(Format('bestmove %s', [LMove]));
    Log.Append(Format('** Move computed in %0.3f s', [LTime / 1000]), TRUE);
  end;
end;

const
  CBoolStr: array[boolean] of string = ('false', 'true');
  
var
  LCmd: string;
  LIdx: integer;
  LMTime, LWTime, LBTime, LMTG, LWInc, LBInc: integer;
  LThread: TSearchThread;
  LDepth: integer;
  LFen: string;
  LBookLine, LBookMove: string;
  LBook: array[boolean] of TTreeList;
  LBookName: array[boolean] of TFileName;
  LColor: boolean;
  LUseBook: boolean;
  
begin
  Randomize;
  LFrc := FALSE;
  LRandMove := (ParamCount = 1) and ((ParamStr(1) = '-r') or (ParamStr(1) = '--random'));
  LBookName[FALSE] := Concat(ExtractFilePath(ParamStr(0)), 'white.txt');
  LBookName[TRUE] := Concat(ExtractFilePath(ParamStr(0)), 'black.txt');
  for LColor := FALSE to TRUE do
  begin
    LBook[LColor] := TTreeList.Create;
    if FileExists(LBookName[LColor]) then
      LBook[LColor].LoadFromFileCompact(LBookName[LColor])
    else
      Log.Append(Format('** File not found: %s', [LBookName[LColor]]));
  end;
  LBookLine := '';
  LUseBook := FALSE;
  
  Send(Format('%s %s', [CAppName, CAppVersion]));
  
  while not Eof do
  begin
    ReadLn(input, LCmd);
    Log.Append(Concat('-> ', LCmd));
    if LCmd = 'quit' then
      Break
    else
      if LCmd = 'uci' then
      begin
        Send(Format('id name %s %s', [CAppName, CAppVersion]), FALSE);
        Send(Format('id author %s', [CAppAuthor]), FALSE);
        Send(Format('option name UCI_Chess960 type check default %s', [CBoolStr[LFrc]]), FALSE);
        Send('uciok');
      end else
        if LCmd = 'isready' then
        begin
          Send('readyok');
        end else
          if LCmd = 'ucinewgame' then
            LPos := CNewPos
          else
            if BeginsWith('position ', LCmd) then
            begin
              if WordPresent('startpos', LCmd) then
              begin
                LPos := EncodePosition;
                LUseBook := not LRandMove;
              end else
                if WordPresent('fen', LCmd) then
                begin
                  LFen := GetFen(LCmd);
                  LPos := EncodePosition(LFen, LFrc);
                  LUseBook := IsUsualStartPos(LFen) and not LRandMove;  
                end else
                  Log.Append(Format('** Unknown command: %s', [LCmd]));
              
              LBookLine := '';
              if WordPresent('moves', LCmd) then
                for LIdx := 4 to WordsNumber(LCmd) do
                begin
                  LMove := GetWord(LIdx, LCmd);
                  if IsChessMove(LMove) then
                  begin
                    if not Move.DoMove(LPos, LMove) then
                      Log.Append(Format('** Impossible move: %s', [LMove]));
                    LBookLine := Concat(LBookLine, ' ', LMove);
                  end;
                end;
            end else
              if BeginsWith('go', LCmd) then
              begin
                if IsGoCmd(LCmd, LWTime, LBTime, LWInc, LBinc) then // go wtime 60000 btime 60000 winc 1000 binc 1000
                  LTimeAv := IfThen(LPos.Side, LBTime div 100 + LBinc, LWTime div 100 + LWInc)
                else
                  if IsGoCmd(LCmd, LWTime, LBTime, LMTG) then       // go wtime 59559 btime 56064 movestogo 38
                    LTimeAv := IfThen(LPos.Side, LBTime div LMTG, LWTime div LMTG)
                  else
                    if IsGoCmd(LCmd, LWTime, LBTime) then           // go wtime 600000 btime 600000
                      LTimeAv := IfThen(LPos.Side, LBTime div 100, LWTime div 100)
                    else
                      if IsGoCmd(LCmd, LMTime) then                 // go movetime 500
                        LTimeAv := LMTime
                      else
                      begin
                        LTimeAv := 1000;
                        Log.Append(Format('** Unknown command: %s', [LCmd]));
                      end;

                if LUseBook then
                begin
                  LBookMove := LBook[LPos.Side].FindMoveToPlay(Trim(LBookLine), TRUE);
                  if LBookMove <> '' then
                  begin
                    Log.Append(Format('** Book %s', [LBookMove]));
                    Send(Format('bestmove %s', [LBookMove]));
                    Continue;
                  end;
                end;

                LThread := TSearchThread.Create(TRUE);
                with LThread do
                begin
                  FreeOnTerminate := TRUE;
                  Priority := tpNormal;
                  Start;
                end;
              end else
                if LCmd = 'stop' then
                begin
                  if Assigned(LThread) then
                    LThread.Terminate;
                  Send(Format('bestmove %s', [LMove]));
                end else
                  if BeginsWith('setoption name UCI_Chess960 value ', LCmd) then
                    LFrc := WordPresent('true', LCmd)
                  else
                    if LCmd = 'board' then
                      Send(PosToText(LPos))
                    else
                      if LCmd = 'moves' then
                        DisplayLegalMoves(LPos)
                      else
                        if IsPerftCmd(LCmd, LDepth) then
                          Start(LPos, IfThen(LDepth < 5, LDepth, 5))
                        else
                          if LCmd = 'help' then
                            Send(
                              'UCI commands:' + LineEnding +
                              '  go movetime <x>' + LineEnding +
                              '  go wtime <x> btime <x>' + LineEnding +
                              '  go wtime <x> btime <x> movestogo <x>' + LineEnding +
                              '  go wtime <x> btime <x> winc <x> binc <x>' + LineEnding +
                              '  isready' + LineEnding +
                              '  position fen <fen> [moves ...]' + LineEnding +
                              '  position startpos [moves ...]' + LineEnding +
                              '  quit' + LineEnding +
                              '  setoption name UCI_Chess960 value <true,false>' + LineEnding +
                              '  stop' + LineEnding +
                              '  uci' + LineEnding +
                              '  ucinewgame' + LineEnding +
                              'Custom commands:' + LineEnding +
                              '  board (display the current board)' + LineEnding +
                              '  help' + LineEnding +
                              '  moves (display legal moves for the current position)' + LineEnding +
                              '  perft <x>'
                            )
                          else
                            Log.Append(Format('** Unknown command: %s', [LCmd]));
  end;
  
  LBook[FALSE].Free;
  LBook[TRUE].Free;
end.
