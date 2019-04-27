
{**
  @abstract(Lecture et écriture d'une position dans une chaîne de caractères au format EPD.)
  Les quatre groupes de caractères d'une chaîne EPD correspondent aux quatre premiers groupes de caractères d'une chaîne @html(<a href="http://kirill-kryukov.com/chess/doc/fen.html">FEN</a>).
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
  CATCR: TCaseRoque = (F1, F8); { Arrivée tour, côté roi. }
  CATCD: TCaseRoque = (D1, D8); { Arrivée tour, côté dame. }
  CDTCR: TCaseRoque = (H1, H8); { Départ  tour, côté roi. }
  CDTCD: TCaseRoque = (A1, A8); { Départ  tour, côté dame. }
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
  CEpdCount = 4;
  CFenCount = 6;
var
  x, y, i: integer;
  c: char;
  LCase: TDamier;
begin
  result := CPositionVierge;
  with TStringList.Create, result do
  begin
    DelimitedText := APos;
    Assert(Count in [CEpdCount, CFenCount]);
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
            LCase := CCaseCoord[x, y];
            if c in ['a'..'z'] then
              Allume(Noires, LCase)
            else
              Allume(Blanches, LCase);
            case UpCase(c) of
              'P': Allume(Pions, LCase);
              'R': Allume(Tours, LCase);
              'N': Allume(Cavaliers, LCase);
              'B': Allume(Fous, LCase); 
              'Q': Allume(Dames, LCase);
              'K':
                begin
                  Allume(Rois, LCase);
                  PositionRoi[c = 'k'] := LCase;
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
      if (Blanches or Noires) and CCaseCoord[x, y] = 0 then
      begin
        n := 0;
        while (x + n <= 7) and ((Blanches or Noires) and CCaseCoord[x + n, y] = 0) do
          Inc(n);
        result := Concat(result, IntToStr(n));
        Inc(x, n);
      end else
      begin
        c := '?';
        if EstAllumee(Pions,     CCaseCoord[x, y]) then c := 'P' else
        if EstAllumee(Tours,     CCaseCoord[x, y]) then c := 'R' else
        if EstAllumee(Cavaliers, CCaseCoord[x, y]) then c := 'N' else
        if EstAllumee(Fous,      CCaseCoord[x, y]) then c := 'B' else
        if EstAllumee(Dames,     CCaseCoord[x, y]) then c := 'Q' else
        if EstAllumee(Rois,      CCaseCoord[x, y]) then c := 'K';
        if EstAllumee(Noires,    CCaseCoord[x, y]) then
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
      s := CNomCase[EnPassant];
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
const
  GRILLE =
    '    A   B   C   D   E   F   G   H'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '8 | %s | %s | %s | %s | %s | %s | %s | %s | 8'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '7 | %s | %s | %s | %s | %s | %s | %s | %s | 7'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '6 | %s | %s | %s | %s | %s | %s | %s | %s | 6'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '5 | %s | %s | %s | %s | %s | %s | %s | %s | 5'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '4 | %s | %s | %s | %s | %s | %s | %s | %s | 4'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '3 | %s | %s | %s | %s | %s | %s | %s | %s | 3'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '2 | %s | %s | %s | %s | %s | %s | %s | %s | 2'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '1 | %s | %s | %s | %s | %s | %s | %s | %s | 1'#13#10 +
    '  +---+---+---+---+---+---+---+---+'#13#10 +
    '    A   B   C   D   E   F   G   H';
var
  x, y: integer;
  c: array[0..7, 0..7] of char;
  i: integer;
  
begin
  for y := 7 downto 0 do
    for x := 0 to 7 do
    begin
      i := 8 * y + x;
      c[x, y] := '?';
      if EstAllumeeIndex(APos.Pions,     i) then c[x, y] := 'p' else
      if EstAllumeeIndex(APos.Tours,     i) then c[x, y] := 'r' else
      if EstAllumeeIndex(APos.Cavaliers, i) then c[x, y] := 'n' else
      if EstAllumeeIndex(APos.Fous,      i) then c[x, y] := 'b' else
      if EstAllumeeIndex(APos.Dames,     i) then c[x, y] := 'q' else
      if EstAllumeeIndex(APos.Rois,      i) then c[x, y] := 'k' else
        c[x, y] := ' ';
      if EstAllumeeIndex(APos.Blanches, i) then
        c[x, y] := UpCase(c[x, y]);
    end;
  result := Format(GRILLE, [
    c[0, 7], c[1, 7], c[2, 7], c[3, 7], c[4, 7], c[5, 7], c[6, 7], c[7, 7],
    c[0, 6], c[1, 6], c[2, 6], c[3, 6], c[4, 6], c[5, 6], c[6, 6], c[7, 6],
    c[0, 5], c[1, 5], c[2, 5], c[3, 5], c[4, 5], c[5, 5], c[6, 5], c[7, 5],
    c[0, 4], c[1, 4], c[2, 4], c[3, 4], c[4, 4], c[5, 4], c[6, 4], c[7, 4],
    c[0, 3], c[1, 3], c[2, 3], c[3, 3], c[4, 3], c[5, 3], c[6, 3], c[7, 3],
    c[0, 2], c[1, 2], c[2, 2], c[3, 2], c[4, 2], c[5, 2], c[6, 2], c[7, 2],
    c[0, 1], c[1, 1], c[2, 1], c[3, 1], c[4, 1], c[5, 1], c[6, 1], c[7, 1],
    c[0, 0], c[1, 0], c[2, 0], c[3, 0], c[4, 0], c[5, 0], c[6, 0], c[7, 0]
  ]);
end;

end.
