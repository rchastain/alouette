
{**
  @abstract(Programme principal du moteur d'échecs UCI.)
  Dialogue avec l'utilisateur au moyen du protocole UCI. }

program Alouette;

uses
  Classes, SysUtils, Journal, Joueur, Echecs, Outils;

{$I version.inc}

procedure Ecrire(const AChaine: string; const AFlush: boolean = TRUE);
begin
  WriteLn(output, AChaine);
  if AFlush then
    Flush(output);
end;

type
  {** Processus de calcul du meilleur coup. }
  TProcessus = class(TThread)
    protected
      procedure Execute; override;
  end;

{** L'action du processus consiste à demander un coup au joueur d'échecs artificiel et à l'envoyer à l'utilisateur. }
procedure TProcessus.Execute;
begin
  Ecrire(Format('bestmove %s', [Joueur.Coup]));
end;

var
  LCommande: ansistring;
  LIndex, LDep, LArr: integer;
  LCoup: string;
  
begin
  Ecrire(Format('%s %s', [CApplication, CVersion]));

  while TRUE do
  begin
    ReadLn(input, LCommande);
    TJournal.Ajoute(Concat('>>> ', LCommande));
    if LCommande = 'quit' then
      Break
    else
      if LCommande = 'uci' then
      begin
        Ecrire(Format('id name %s %s', [CApplication, CVersion]), FALSE);
        Ecrire(Format('id author %s', [CAuteur]), FALSE);
        Ecrire('option name UCI_Chess960 type check default false', FALSE);
        Ecrire('uciok');
      end else
        if LCommande = 'isready' then
        begin
          Ecrire('readyok');
        end else
          if LCommande = 'ucinewgame' then
            Joueur.Oublie
          else
            if CommencePar('position ', LCommande) then
            begin
              if Contient('startpos', LCommande) then
                Joueur.PositionDepart
              else if Contient('fen', LCommande) then
                Joueur.NouvellePosition(ExtraitEpd(LCommande));
              
              if Contient('moves', LCommande) then
                for LIndex := 4 to NombreMots(LCommande) do
                begin
                  LCoup := Extrait(LIndex, LCommande);
                  if DecodeChaineCoup(LCoup, LDep, LArr) then
                    Joueur.Rejoue(LCoup);
                end;
            end else
              if CommencePar('go', LCommande) then
              with TProcessus.Create(TRUE) do
              begin
                FreeOnTerminate := TRUE;
                Priority := tpHigher;
                Start;
              end else
                if CommencePar('setoption name UCI_Chess960 value ', LCommande) then
                  ActiveEchecs960(Contient('true', LCommande))
                else
                  if LCommande = 'voir' then
                    Ecrire(VoirPosition(PositionCourante));
  end;
end.
