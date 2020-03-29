
{**
  @abstract(Programme principal du moteur d'échecs.)
  Interface UCI du moteur d'échecs.
}

program Alouette;

uses
{$IFDEF UNIX}
  CWString,
  CThreads,
{$ENDIF}
  Classes,
  SysUtils,
  Math,
  Log,
  Player,
  Chess,
  Utils,
  Perft,
  Settings,
  TreeList;

{$I version.inc}
  
procedure Send(const AMessage: string; const AFlush: boolean = TRUE);
begin
  WriteLn(output, AMessage);
  if AFlush then
    Flush(output);
  Log.Append(Concat('< ', AMessage));
end;

type
  {** Processus de recherche du meilleur coup. }
  TSearchThread = class(TThread)
    protected
      procedure Execute; override;
  end;

var
  LTimeAvailable: cardinal;

{** L'action du processus consiste à demander un coup au joueur d'échecs artificiel et à l'envoyer à l'utilisateur. }
procedure TSearchThread.Execute;
var
  LTimeUsed: cardinal;
  LMove: string;
begin
  LTimeUsed := GetTickCount64;
  LMove := Player.BestMove(LTimeAvailable);
  LTimeUsed := GetTickCount64 - LTimeUsed;
  if not Terminated then
  begin
    Send(Format('bestmove %s', [LMove]));
    Log.Append(Format('Meilleur coup trouvé en %0.3f s.', [LTimeUsed / 1000]), TRUE);
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
  LBookLine, LBookMove: string;
  LBook: array[boolean] of TTreeList;
  LBookName: array[boolean] of TFileName;
  LColor: boolean;
  
begin
  Randomize;

  LoadSettings(LVariantInitialValue);
  if not SettingsFileExists then
    SaveSettings(LVariantInitialValue);
  SetVariant(LVariantInitialValue);
  
  LBookName[FALSE] := Concat(ExtractFilePath(ParamStr(0)), 'white.txt');
  LBookName[TRUE] := Concat(ExtractFilePath(ParamStr(0)), 'black.txt');
  for LColor := FALSE to TRUE do
  begin
    LBook[LColor] := TTreeList.Create;
    if FileExists(LBookName[LColor]) then
      LBook[LColor].LoadFromFileCompact(LBookName[LColor])
    else
      Log.Append(Format('Fichier introuvable (%s).', [LBookName[LColor]]));
  end;
  LBookLine := '';
  
  Send(Format('%s %s', [CAppName, CAppVersion]));
  while not Eof do
  begin
    ReadLn(input, LCmd);
    Log.Append(Concat('> ', LCmd));
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
                Player.LoadStartPosition
              else if WordPresent('fen', LCmd) then
                Player.SetPosition(GetFen(LCmd))
              else
                Log.Append(Format('Commande non reconnue (%s).', [LCmd]));
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
                  LTimeAvailable := IfThen(LPos.SideToMove, LBinc, LWInc)
                else
                  if IsGoCmd(LCmd, LWTime, LBTime, LMTG) then       // go wtime 59559 btime 56064 movestogo 38
                    LTimeAvailable := IfThen(LPos.SideToMove, LBTime div LMTG, LWTime div LMTG)
                  else
                    if IsGoCmd(LCmd, LWTime, LBTime) then           // go wtime 600000 btime 600000
                      LTimeAvailable := IfThen(LPos.SideToMove, LBTime, LWTime)
                    else
                      if IsGoCmd(LCmd, LMTime) then                 // go movetime 500
                        LTimeAvailable := LMTime
                      else
                        Log.Append(Format('Commande non reconnue (%s).', [LCmd]));
                
                if not CurrentVariant then
                begin
                  LBookMove := LBook[LPos.SideToMove].FindMoveToPlay(Trim(LBookLine), TRUE);
                  if LBookMove <> '' then
                  begin
                    Log.Append(Format('Coup trouvé dans le livre : %s', [LBookMove]));
                    Send(Format('bestmove %s', [LBookMove]));
                    Continue;
                  end;
                end;
                
                LThread := TSearchThread.Create(TRUE);
                with LThread do
                begin
                  FreeOnTerminate := TRUE;
                  //Priority := tpHigher;
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
                    if LCmd = 'show' then
                      Send(PositionToText(CurrentPosition))
                    else
                      if IsPerftCmd(LCmd, LDepth) then
                        Start(CurrentPosition, LDepth)
                      else
                        if LCmd = 'help' then
                          Send(
                            'go movetime <x>' + LineEnding +
                            'go wtime <x> btime <x>' + LineEnding +
                            'go wtime <x> btime <x> movestogo <x>' + LineEnding +
                            'go wtime <x> btime <x> winc <x> binc <x>' + LineEnding +
                            'help' + LineEnding +
                            'isready' + LineEnding +
                            'perft <x>' + LineEnding +
                            'position fen <fen> [moves ...]' + LineEnding +
                            'position startpos [moves ...]' + LineEnding +
                            'quit' + LineEnding +
                            'setoption name UCI_Chess960 value <true,false>' + LineEnding +
                            'show' + LineEnding +
                            'stop' + LineEnding +
                            'uci' + LineEnding +
                            'ucinewgame'
                          )
                        else
                          Log.Append(Format('Commande non reconnue (%s).', [LCmd]));
  end;
  
  LBook[FALSE].Free;
  LBook[TRUE].Free;
end.
