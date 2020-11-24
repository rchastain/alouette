
{**
  @abstract(Programme principal du moteur d'échecs.)
  Interface UCI du moteur d'échecs.
}

program Alouette;

uses
{$IFDEF UNIX}
  CWString, CThreads,
{$ENDIF}
  Classes, SysUtils, Math,
  Log, Player, Chess, Utils, Perft, Settings, Trees;

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
  {** Processus de recherche du meilleur coup. }
  TSearchThread = class(TThread)
    protected
      procedure Execute; override;
  end;

var
  LTimeAvailable: cardinal;
  LRandomMove: boolean;

{** L'action du processus consiste à demander un coup au joueur d'échecs artificiel et à l'envoyer à l'utilisateur. }
procedure TSearchThread.Execute;
var
  LTimeUsed: cardinal;
  LMove: string;
begin
  LTimeUsed := GetTickCount64;
  LMove := Player.BestMove(LTimeAvailable, LRandomMove);
  LTimeUsed := GetTickCount64 - LTimeUsed;
  if not Terminated then
  begin
    Send(Format('bestmove %s', [LMove]));
    Log.Append(Format('** Move computed in %0.3f s', [LTimeUsed / 1000]), TRUE);
  end;
end;

const
  CBoolStr: array[boolean] of string = ('false', 'true');
  
var
  LCmd: string;
  LIdx: integer;
  LMove: string;
  LMTime, LWTime, LBTime, LMTG, LWInc, LBInc: integer;
  LPos: TPosition;
  LVariantInitialValue: boolean;
  LThread: TSearchThread;
  LDepth: integer;
  LFen: string;
  LBookLine, LBookMove: string;
  LBook: array[boolean] of TTreeList;
  LBookName: array[boolean] of TFileName;
  LColor: boolean;
  LCanUseBook: boolean;
  
begin
  Randomize;

  LoadSettings(LVariantInitialValue);
  if not SettingsFileExists then
    SaveSettings(LVariantInitialValue);
  SetVariant(LVariantInitialValue);
  
  LRandomMove := (ParamCount = 1) and ((ParamStr(1) = '-r') or (ParamStr(1) = '--random'));
  
  if not LRandomMove then
  begin
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
    LCanUseBook := FALSE;
  end;
  
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
        Send(Format('option name UCI_Chess960 type check default %s', [CBoolStr[CurrentVariant]]), FALSE);
        Send('uciok');
      end else
        if LCmd = 'isready' then
        begin
          Send('readyok');
        end else
          if LCmd = 'ucinewgame' then
            Player.Reset
          else
            if BeginsWith('position ', LCmd) then
            begin
              if WordPresent('startpos', LCmd) then
              begin
                Player.LoadStartPosition;
                LCanUseBook := not LRandomMove;
              end else
                if WordPresent('fen', LCmd) then
                begin
                  LFen := GetFen(LCmd);
                  Player.SetPosition(LFen);
                  LCanUseBook := IsConventionalStartPosition(LFen) and not LRandomMove;  
                end else
                  Log.Append(Format('** Unknown command: %s', [LCmd]));
              
              LBookLine := '';
              if WordPresent('moves', LCmd) then
                for LIdx := 4 to WordsNumber(LCmd) do
                begin
                  LMove := GetWord(LIdx, LCmd);
                  if IsChessMove(LMove) then
                  begin
                    Player.DoMove(LMove);
                    LBookLine := Concat(LBookLine, ' ', LMove);
                  end;
                end;
            end else
              if BeginsWith('go', LCmd) then
              begin
                LPos := Player.CurrentPosition;
                if IsGoCmd(LCmd, LWTime, LBTime, LWInc, LBinc) then // go wtime 60000 btime 60000 winc 1000 binc 1000
                  LTimeAvailable := IfThen(LPos.Side, LBTime div 100 + LBinc, LWTime div 100 + LWInc)
                else
                  if IsGoCmd(LCmd, LWTime, LBTime, LMTG) then       // go wtime 59559 btime 56064 movestogo 38
                    LTimeAvailable := IfThen(LPos.Side, LBTime div LMTG, LWTime div LMTG)
                  else
                    if IsGoCmd(LCmd, LWTime, LBTime) then           // go wtime 600000 btime 600000
                      LTimeAvailable := IfThen(LPos.Side, LBTime div 100, LWTime div 100)
                    else
                      if IsGoCmd(LCmd, LMTime) then                 // go movetime 500
                        LTimeAvailable := LMTime
                      else
                      begin
                        LTimeAvailable := 1000;
                        Log.Append(Format('** Unknown command: %s', [LCmd]));
                      end;

                if LCanUseBook then
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
                  Send(Format('bestmove %s', [Player.InstantMove]));
                end else
                  if BeginsWith('setoption name UCI_Chess960 value ', LCmd) then
                    SetVariant(WordPresent('true', LCmd))
                  else
                    if LCmd = 'board' then
                      Send(PositionToText(CurrentPosition))
                    else
                      if IsPerftCmd(LCmd, LDepth) then
                        Start(CurrentPosition, LDepth)
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
                            '  board (displays the current board)' + LineEnding +
                            '  help' + LineEnding +
                            '  perft <x>'
                          )
                        else
                          Log.Append(Format('** Unknown command: %s', [LCmd]));
  end;
  
  if not LRandomMove then
  begin
    LBook[FALSE].Free;
    LBook[TRUE].Free;
  end;
end.
