
{**
  @abstract(Programme principal du moteur d'échecs UCI.)
  Dialogue avec l'utilisateur au moyen du protocole UCI.
}

program Alouette;

uses
{$IFDEF UNIX}
  cthreads,
  cwstring,
{$ENDIF}
  Classes,
  SysUtils,
  Math,
  Journal,
  Joueur,
  Echecs,
  Outils,
  Utils,
{$IFDEF DEBUG}
  Essais,
{$ENDIF}
  Performance;

{$I version.inc}
  
procedure Ecrire(const AChaine: string; const ATerminer: boolean = TRUE);
begin
  WriteLn(output, AChaine);
  if ATerminer then
    Flush(output);
end;

type
  {** Processus de calcul du meilleur coup. }
  TProcessus = class(TThread)
    protected
      procedure Execute; override;
  end;

var
  GTempsDisponible: cardinal;
  
{** L'action du processus consiste à demander un coup au joueur d'échecs artificiel et à l'envoyer à l'utilisateur. }
procedure TProcessus.Execute;
begin
  Ecrire(Format('bestmove %s', [Joueur.Coup(GTempsDisponible)]));
end;
  
var
  LCmd: ansistring;
  LIdx: integer;
  LCoup: string;
  LMTime, LWTime, LBTime, LMTG, LWInc, LBInc: integer;
  LPos: TPosition;
  
begin
  Ecrire(Format('%s %s', [CApp, CVer]));
  while not EOF do
  begin
    ReadLn(input, LCmd);
    TJournal.Ajoute(Concat('>>> ', LCmd));
    if LCmd = 'quit' then
      Break
    else
      if LCmd = 'uci' then
      begin
        Ecrire(Format('id name %s %s', [CApp, CVer]), FALSE);
        Ecrire(Format('id author %s', [CAut]), FALSE);
        Ecrire('option name UCI_Chess960 type check default false', FALSE);
        Ecrire('uciok');
      end else
        if LCmd = 'isready' then
        begin
          Ecrire('readyok');
        end else
          if LCmd = 'ucinewgame' then
            Joueur.Oublie
          else
            if CommencePar('position ', LCmd) then
            begin
              if Contient('startpos', LCmd) then
                Joueur.PositionDepart
              else if Contient('fen', LCmd) then
                Joueur.NouvellePosition(ExtraitEpd(LCmd));
              if Contient('moves', LCmd) then
                for LIdx := 4 to NombreMots(LCmd) do
                begin
                  LCoup := Extrait(LIdx, LCmd);
                  if DecodeChaineCoup(LCoup) then
                    Joueur.Rejoue(LCoup);
                end;
            end else
              if BeginsWith('go', LCmd) then
              begin
                LPos := Joueur.PositionCourante;
                if IsGoCmd(LCmd, LWTime, LBTime, LWInc, LBinc) then // go wtime 60000 btime 60000 winc 1000 binc 1000
                  GTempsDisponible := IfThen(LPos.Trait, LBinc, LWInc)
                else
                if IsGoCmd(LCmd, LWTime, LBTime, LMTG) then         // go wtime 59559 btime 56064 movestogo 38
                  GTempsDisponible := IfThen(LPos.Trait, LBTime div LMTG, LWTime div LMTG)
                else
                if IsGoCmd(LCmd, LWTime, LBTime) then               // go wtime 600000 btime 600000
                  GTempsDisponible := IfThen(LPos.Trait, LBTime, LWTime)
                else
                if IsGoCmd(LCmd, LMTime) then                       // go movetime 500
                  GTempsDisponible := LMTime
                else
                  Assert(FALSE);
                
                with TProcessus.Create(TRUE) do
                begin
                  FreeOnTerminate := TRUE;
                  Priority := tpHigher;
                  Start;
                end;
              end else
                  if CommencePar('setoption name UCI_Chess960 value ', LCmd) then
                    ActiveEchecs960(Contient('true', LCmd))
                  else
                    if LCmd = 'show' then
                      Ecrire(VoirPosition(PositionCourante))
                    else
                    if LCmd = 'perft' then
                      EssaiPerf();
  end;
end.
