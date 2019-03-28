
{**
  @abstract(Programme principal du moteur d'échecs UCI.)
  Dialogue avec l'utilisateur au moyen du protocole UCI. }

program Alouette;

uses
  Classes, SysUtils, StrUtils, Journal, Utiles, Joueur;

{$I version.inc}

type
  {** Processus indépendant pour le calcul du meilleur coup. }
  TProcessus = class(TThread)
    protected
      procedure Execute; override;
  end;

procedure TProcessus.Execute;
begin
  WriteLn(output, Format('bestmove %s', [Joueur.Coup]));
  Flush(output);
end;

var
  LCommande: ansistring;
  LIndex, LDepart, LArrivee: integer;
  LCoup: string;
  
begin
  WriteLn(output, Format('%s v%s', [CApplication, CVersion]));
  Flush(output);
  TJournal.Ajoute(Format('%s v%s %s %s', [CApplication, CVersion, ChaineDateHeure, ChaineCompilateur]));

  while TRUE do
  begin
    ReadLn(input, LCommande);
    TJournal.Ajoute(Concat('>>> ', LCommande));
    if LCommande = 'quit' then
      Break
    else
      if LCommande = 'uci' then
      begin
        WriteLn(output, Format('id name %s %s', [CApplication, CVersion]));
        WriteLn(output, Format('id author %s', [CAuteur]));
        WriteLn(output, 'option name UCI_Chess960 type check default false');
        WriteLn(output, 'uciok');
        Flush(output);
      end else
        if LCommande = 'isready' then
        begin
          WriteLn(output, 'readyok');
          Flush(output);
        end else
          if LCommande = 'ucinewgame' then
            Joueur.Oublie
          else
            if Pos('position', LCommande) = 1 then
            begin
              if IsWordPresent('startpos', LCommande, [' ']) then
                Joueur.PositionDepart
              else if IsWordPresent('fen', LCommande, [' ']) then
                Joueur.NouvellePosition(Format('%s %s %s %s', [
                  ExtractWord(3, LCommande, [' ']),
                  ExtractWord(4, LCommande, [' ']),
                  ExtractWord(5, LCommande, [' ']),
                  ExtractWord(6, LCommande, [' '])
                ]));
              if IsWordPresent('moves', LCommande, [' ']) then
                for LIndex := 4 to WordCount(LCommande, [' ']) do
                begin
                  LCoup := ExtractWord(LIndex, LCommande, [' ']);
                  if DecodeChaineCoup(LCoup, LDepart, LArrivee) then
                    Joueur.Rejoue(LCoup);
                end;
            end else
              if Pos('go', LCommande) = 1 then
              with TProcessus.Create(TRUE) do
              begin
                FreeOnTerminate := TRUE;
                Priority := tpHigher;
                Start;
              end else
                if Pos('setoption name UCI_Chess960 value', LCommande) = 1 then
                  if Pos('false', LCommande) > 0 then
                    ActiveMode960(FALSE)
                  else if Pos('true', LCommande) > 0 then
                    ActiveMode960(TRUE);
                
  end;
end.
