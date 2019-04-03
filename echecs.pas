
{**
@abstract(Lecture et écriture d'une position dans une chaîne de caractères au format EPD.)
Les quatre groupes de caractères d'une chaîne au format EPD sont identiques aux quatre premiers groupes de caractères d'une chaîne au format @html(<a href="http://kirill-kryukov.com/chess/doc/fen.html">FEN</a>).
}
unit Echecs;

interface

uses
  Damier, Tables;

const
  CBlanc = FALSE;
  CNoir = TRUE;
  CNeant = -1;
  
type
  TDonneesRoque = record
    XTourRoi,
    XTourDame: integer;
  end;
  TRoque = array[boolean] of TDonneesRoque;
  TPosition = record
    Blanches,
    Noires,
    Pions,
    Tours,
    Cavaliers,
    Fous,
    Dames,
    Rois: TDamier;
    Trait: boolean;
    Roque: TRoque;
    EnPassant: integer;
    PositionRoi: array[boolean] of TDamier;
  end;
  
const
  CPositionVierge: TPosition = (
    Blanches: 0;
    Noires: 0;
    Pions: 0;
    Tours: 0;
    Cavaliers: 0;
    Fous: 0;
    Dames: 0;
    Rois: 0;
    Trait: FALSE;
    Roque: (
      (XTourRoi: CNeant; XTourDame: CNeant),
      (XTourRoi: CNeant; XTourDame: CNeant)
    );
    EnPassant: CNeant;
    PositionRoi: (0, 0)
  );

const
  CPositionDepart = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -';
type
  TCaseRoque = array[boolean] of integer;
const
  { Arrivée tour, côté roi. }
  CATCR: TCaseRoque = (F1, F8);
  { Arrivée tour, côté dame. }
  CATCD: TCaseRoque = (D1, D8);
  { Départ tour, côté roi. }
  CDTCR: TCaseRoque = (H1, H8);
  { Départ tour, côté dame. }
  CDTCD: TCaseRoque = (A1, A8);
  CColC = 2;
  CColD = 3;
  CColE = 4;
  CColF = 5;
  CColG = 6;
  CLig8 = 7;
  CLig1 = 0;
  CLigRoq: array[boolean] of integer = (0, 7);
  
function EncodePosition(const APos: string = CPositionDepart; const AEchecs960: boolean = FALSE): TPosition;
function DecodePosition(const APos: TPosition; const AEchecs960: boolean = FALSE): string;
function Colonne(const ACase: TDamier): integer;
function VoirPosition(const APos: TPosition): string;

implementation

uses
  SysUtils, Classes, Journal;

procedure Reinitialise(var ARoque: TRoque);
var
  LCouleur: boolean;
begin
  for LCouleur := CBlanc to CNoir do
  with ARoque[LCouleur] do
  begin
    XTourRoi := CNeant;
    XTourDame := CNeant;
  end;
end;

function DecodeChaineRoqueTradition(const AChaine: string): TRoque;
begin
  Reinitialise(result);
  if Pos('K', aChaine) > 0 then result[CBlanc].XTourRoi := 7;
  if Pos('Q', aChaine) > 0 then result[CBlanc].XTourDame := 0;
  if Pos('k', aChaine) > 0 then result[CNoir].XTourRoi := 7;
  if Pos('q', aChaine) > 0 then result[CNoir].XTourDame := 0;
end;

function DecodeChaineRoque(const AChaine: string; const AXRoiBlanc: integer; const AXRoiNoir: integer): TRoque;
const
  CLettre: array[boolean, 0..7] of char = (
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
    ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
  );
var
  c1, c2: char;
  b: boolean;
  a: byte;
begin
  Reinitialise(result);
  for b := CBlanc to CNoir do
  begin
    a := Ord(CLettre[b, 0]);
    if b then c1 := Chr(AXRoiBlanc + a) else c1 := Chr(AXRoiNoir + a);
    for c2 := CLettre[b, 7] downto Succ(c1) do if Pos(c2, aChaine) > 0 then result[b].XTourRoi := Ord(c2) - a;
    for c2 := CLettre[b, 0] to Pred(c1) do if Pos(c2, aChaine) > 0 then result[b].XTourDame := Ord(c2) - a;
  end;
end;

function EncodeChaineRoque(const ARoque: TRoque; const AEchecs960: boolean = FALSE): string;
begin
  result := '';
  if AEchecs960 then
  begin
    with ARoque[CBlanc] do begin
    if (XTourRoi >= 0) and (XTourRoi <= 7) then result := Chr(XTourRoi + Ord('A'));
    if (XTourDame >= 0) and (XTourDame <= 7) then result := Concat(result, Chr(XTourDame + Ord('A')));
    end;
    with ARoque[CNoir] do begin
    if (XTourRoi >= 0) and (XTourRoi <= 7) then result := Concat(result, Chr(XTourRoi + Ord('a')));
    if (XTourDame >= 0) and (XTourDame <= 7) then result := Concat(result, Chr(XTourDame + Ord('a')));
    end;
  end else
  begin
    if ARoque[CBlanc].XTourRoi = 7 then result := 'K';
    if ARoque[CBlanc].XTourDame = 0 then result := Concat(result, 'Q');
    if ARoque[CNoir].XTourRoi = 7 then result := Concat(result, 'k');
    if ARoque[CNoir].XTourDame = 0 then result := Concat(result, 'q');
  end;
  if result = '' then
    result := '-';
end;

function Colonne(const ACase: TDamier): integer;
var
  x: integer;
begin
  result := CNeant;
  x := 0;
  while (result = CNeant) and (x <= 7) do
    if EstAllumee(CColonne[x], ACase) then
      result := x
    else
      Inc(x);
end;

const
  CSymboleTrait: array[boolean] of char = ('w', 'b');
  
function EncodePosition(const APos: string; const AEchecs960: boolean): TPosition;
const
  CEpdNum = 4;
  CFenNum = 6;
var
  x, y, i: integer;
  c: char;
  cb: TDamier;
begin
  result := CPositionVierge;
  with TStringList.Create, result do
  begin
    DelimitedText := APos;
    Assert(Count in [CEpdNum, CFenNum]);
    begin
      x := 0;
      y := 7;
      i := 1;
      while i <= Length(Strings[0]) do
      begin
        c := Strings[0][i];
        case c of
          '/':
            begin
              x := 0;
              Dec(y);
            end;
          '1'..'8':
            while c > '0' do
            begin
              Inc(x);
              Dec(c);
            end;
        else
          begin
            cb := CCaseXY[x, y];
            if c in ['a'..'z'] then
              Allume(Noires, cb)
            else
              Allume(Blanches, cb);
            case UpCase(c) of
              'P': Allume(Pions, cb);
              'R': Allume(Tours, cb);
              'N': Allume(Cavaliers, cb);
              'B': Allume(Fous, cb); 
              'Q': Allume(Dames, cb);
              'K':
                begin
                  Allume(Rois, cb);
                  PositionRoi[c = 'k'] := cb;
                end;
            end;
            Inc(x);
          end;
        end;
        Inc(i);
      end;
      Trait := Strings[1] = CSymboleTrait[CNoir];
      if AEchecs960 then
        Roque := DecodeChaineRoque(Strings[2], Colonne(PositionRoi[CBlanc]), Colonne(PositionRoi[CNoir]))
      else
        Roque := DecodeChaineRoqueTradition(Strings[2]);
      if Strings[3] = '-' then
        EnPassant := CNeant
      else
        EnPassant := DecodeNomCase(Strings[3]);
    end;
    Free;
  end;
end;

function DecodePosition(const APos: TPosition; const AEchecs960: boolean): string;
var
  x, y, n: integer;
  c: char;
  s: string;
begin
  result := '';
  with APos do
  begin
    x := 0;
    y := 7;
    while y >= 0 do
    begin
      if (Blanches or Noires) and CCaseXY[x, y] = 0 then
      begin
        n := 0;
        while (x + n <= 7) and ((Blanches or Noires) and CCaseXY[x + n, y] = 0) do
          Inc(n);
        result := Concat(result, IntToStr(n));
        Inc(x, n);
      end else
      begin
        c := '?';
        if (Pions and CCaseXY[x, y]) = CCaseXY[x, y] then
          c := 'P' else
        if (Tours and CCaseXY[x, y]) = CCaseXY[x, y] then
          c := 'R' else
        if (Cavaliers and CCaseXY[x, y]) = CCaseXY[x, y] then
          c := 'N' else
        if (Fous and CCaseXY[x, y]) = CCaseXY[x, y] then
          c := 'B' else
        if (Dames and CCaseXY[x, y]) = CCaseXY[x, y] then
          c := 'Q' else
        if (Rois and CCaseXY[x, y]) = CCaseXY[x, y] then
          c := 'K';
        if (Noires and CCaseXY[x, y]) = CCaseXY[x, y] then
          c := Chr(Ord(c) + 32);
        result := Concat(result, c);
        Inc(x);
      end;
      if x > 7 then
      begin
        if y > 0 then
          result := Concat(result, '/');
        x := 0;
        Dec(y);
      end;
    end;
    if EnPassant = CNeant then
      s := '-'
    else
      s := NomCase(EnPassant);
    result := Format(
      '%s %s %s %s',
      [
        result,
        CSymboleTrait[Trait],
        EncodeChaineRoque(Roque, AEchecs960),
        s
      ]
    );
  end;
end;

function VoirPosition(const APos: TPosition): string;
var
  x, y: integer;
  c: char;
  i: integer;
begin
  result := '+    a b c d e f g h    +'#13#10#13#10;
  for y := 7 downto 0 do
  begin
    result := Concat(result, IntToStr(Succ(y)), '   ');
    for x := 0 to 7 do
    begin
      i := 8 * y + x;
      c := '?';
      if EstAllumee_(APos.Pions, i) then
        c := 'p'
      else if EstAllumee_(APos.Tours, i) then
        c := 'r'
      else if EstAllumee_(APos.Cavaliers, i) then
        c := 'n'
      else if EstAllumee_(APos.Fous, i) then
        c := 'b'
      else if EstAllumee_(APos.Dames, i) then
        c := 'q'
      else if EstAllumee_(APos.Rois, i) then
        c := 'k'
      else
        c := '.';
      if EstAllumee_(APos.Blanches, i) then
        c := UpCase(c);
      result := Concat(result, ' ', c);
    end;
    result := Concat(result, '    ', IntToStr(Succ(y)), #13#10);
  end;
  result := Concat(result, #13#10'+    a b c d e f g h    +');
end;

end.
