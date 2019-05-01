
{**
  @abstract(Damier binaire.)
  Représentation d'un damier par un nombre entier de 64 chiffres binaires.
}

unit Damier;

interface

type
  {** Le damier est représenté par un nombre entier de 64 chiffres binaires. }
  TDamier = Int64;
  TPiece = (PionBlanc, PionNoir, Tour, Cavalier, Fou, Dame, Roi);

{** Case pour un nombre donné de 0 à 63. La fonction renvoie un damier avec un seul chiffre allumé. }
function FCase(const AIdx: integer): TDamier; overload;
{** Case pour deux nombres donnés de 0 à 7. }
function FCase(const ACol, ALig: integer): TDamier; overload;
function FIndex(const ACol, ALig: integer): integer;
function NomCase(const ACol, ALig: integer; const AMaj: boolean = FALSE): string; overload;
function NomCase(const AIdx: integer; const AMaj: boolean = FALSE): string; overload;
function NomCoup(const ADep, AArr: integer): string; overload;
function NomCoup(const ACoup: integer): string; overload;
function EncodeCoup(const i, j: integer): integer;
procedure DecodeCoup(const ACoup: integer; out ADep, AArr: integer);
{** Convertit une chaîne de la forme "a1" en un nombre de 0 à 63. }
function DecodeNomCase(const ANom: string): integer;
{** Pour savoir si une case est allumée dans un damier. }
function EstAllumee(const ADam, ACase: TDamier): boolean;
{** Allume une case dans un damier. }
procedure Allume(var ADam: TDamier; const ACase: TDamier);
{** Éteint une case. }
procedure Eteint(var ADam: TDamier; const ACase: TDamier);
{** Éteint une case et en allume une autre. }
procedure Deplace(var AType, ACoul: TDamier; const ACaseDep, ACaseArr: TDamier);
{** Chaîne de chiffres binaires. }
function FChaine(const ADam: TDamier): string;
{** Chaîne de chiffres binaires en forme de damier. }
function FChaineDamier(const ADam: TDamier): string;
{** Pour savoir si la nature d'une pièce lui permet tel déplacement. }
function Possible(const APiece: TPiece; const Ax1, Ay1, Ax2, Ay2: integer): boolean;
{** Toutes les cases que la pièce, selon son type, peut atteindre. }
function Cibles(const APiece: TPiece; const AIdx: integer): TDamier;
{** Les cases à survoler pour aller d'un endroit à un autre. }
function Chemin(const ACase1, ACase2: integer): TDamier;

const  
  {** Numérotation des cases de 0 à 63. }
  A1 = 00; B1 = 01; C1 = 02; D1 = 03; E1 = 04; F1 = 05; G1 = 06; H1 = 07;
  A2 = 08; B2 = 09; C2 = 10; D2 = 11; E2 = 12; F2 = 13; G2 = 14; H2 = 15;
  A3 = 16; B3 = 17; C3 = 18; D3 = 19; E3 = 20; F3 = 21; G3 = 22; H3 = 23;
  A4 = 24; B4 = 25; C4 = 26; D4 = 27; E4 = 28; F4 = 29; G4 = 30; H4 = 31;
  A5 = 32; B5 = 33; C5 = 34; D5 = 35; E5 = 36; F5 = 37; G5 = 38; H5 = 39;
  A6 = 40; B6 = 41; C6 = 42; D6 = 43; E6 = 44; F6 = 45; G6 = 46; H6 = 47;
  A7 = 48; B7 = 49; C7 = 50; D7 = 51; E7 = 52; F7 = 53; G7 = 54; H7 = 55;
  A8 = 56; B8 = 57; C8 = 58; D8 = 59; E8 = 60; F8 = 61; G8 = 62; H8 = 63;
  
  CNomCase: array [0..63] of string[2] = (
    'a1', 'b1', 'c1', 'd1', 'e1', 'f1', 'g1', 'h1',
    'a2', 'b2', 'c2', 'd2', 'e2', 'f2', 'g2', 'h2',
    'a3', 'b3', 'c3', 'd3', 'e3', 'f3', 'g3', 'h3',
    'a4', 'b4', 'c4', 'd4', 'e4', 'f4', 'g4', 'h4',
    'a5', 'b5', 'c5', 'd5', 'e5', 'f5', 'g5', 'h5',
    'a6', 'b6', 'c6', 'd6', 'e6', 'f6', 'g6', 'h6',
    'a7', 'b7', 'c7', 'd7', 'e7', 'f7', 'g7', 'h7',
    'a8', 'b8', 'c8', 'd8', 'e8', 'f8', 'g8', 'h8'
  );

implementation

uses
  SysUtils;
  
function FCase(const AIdx: integer): TDamier;
begin
  Assert((AIdx >= 0) and (AIdx <= 63));
  result := TDamier(1) shl AIdx;
end;

function FCase(const ACol, ALig: integer): TDamier;
begin
  Assert((ACol >= 0) and (ACol <= 7) and (ALig >= 0) and ( ALig <= 7));
  result := FCase(8 * ALig + ACol);
end;

function FIndex(const ACol, ALig: integer): integer;
begin
  result := 8 * ALig + ACol;
end;

function NomCase(const ACol, ALig: integer; const AMaj: boolean): string;
begin
  Assert((ACol >= 0) and (ACol <= 7) and (ALig >= 0) and ( ALig <= 7));
  result := Concat(
    Chr(ACol + Ord('a') + (Ord('A') - Ord('a')) * Ord(AMaj)),
    Chr(ALig + Ord('1'))
  );
end;

function NomCase(const AIdx: integer; const AMaj: boolean): string;
begin
  result := NomCase(AIdx mod 8, AIdx div 8, AMaj);
end;

function NomCoup(const ADep, AArr: integer): string;
begin
  result := Concat(NomCase(ADep), NomCase(AArr));
end;

function EncodeCoup(const i, j: integer): integer;
begin
  result := 100 * i + j;
end;

procedure DecodeCoup(const ACoup: integer; out ADep, AArr: integer);
begin
  ADep := ACoup div 100;
  AArr := ACoup mod 100;
end;

function NomCoup(const ACoup: integer): string;
var
  LDepart, LArriv: integer;
begin
  DecodeCoup(ACoup, LDepart, LArriv);
  result := Concat(NomCase(LDepart), NomCase(LArriv));
end;

function DecodeNomCase(const ANom: string): integer;
begin
  Assert((Length(ANom) = 2) and (ANom[1] in ['a'..'h']) and (ANom[2] in ['1'..'8']));
  result := 8 * (Ord(ANom[2]) - Ord('1')) + (Ord(ANom[1]) - Ord('a'));
end;

function EstAllumee(const ADam, ACase: TDamier): boolean;
begin
  Assert(ACase <> 0);
  result := (ADam and ACase) = ACase;
end;

procedure Allume(var ADam: TDamier; const ACase: TDamier);
begin
  ADam := ADam or ACase;
end;

procedure Eteint(var ADam: TDamier; const ACase: TDamier);
begin
  ADam := ADam and not ACase;
end;

procedure Deplace(var AType, ACoul: TDamier; const ACaseDep, ACaseArr: TDamier);
begin
  AType := AType and not ACaseDep or ACaseArr;
  ACoul := ACoul and not ACaseDep or ACaseArr;
end;

function FChaine(const ADam: TDamier): string;
const
  CCaractere: array[boolean] of char = ('0', '1');
var
  i: integer;
begin
  SetLength(result, 64);
  for i := 63 downto 0 do
    result[64 - i] := CCaractere[EstAllumee(ADam, FCase(i))];
end;

function FChaineDamier(const ADam: TDamier): string;
var
  x, y: integer;
  LDamier: string;
begin
  LDamier := FChaine(ADam);
  result := '+   abcdefgh   +'#13#10#13#10;
  for y := 7 downto 0 do
  begin
    result := Concat(result, IntToStr(Succ(y)), '   ');
    for x := 0 to 7 do
      result := Concat(result, LDamier[64 - 8 * y - x]);
    result := Concat(result, '   ', IntToStr(Succ(y)), #13#10);
  end;
  result := Concat(result, #13#10'+   abcdefgh   +');
end;

function Possible(const APiece: TPiece; const Ax1, Ay1, Ax2, Ay2: integer): boolean;
var
  dx, dy, adx, ady: integer;
begin
  dx := Ax2 - Ax1;
  dy := Ay2 - Ay1;
  adx := Abs(dx);
  ady := Abs(dy);
  case APiece of
    PionBlanc:
      result := (dy = 1) and (adx = 1);
    PionNoir:
      result := (dy = -1) and (adx = 1);
    Tour:
      result := (dx = 0) xor (dy = 0);
    Cavalier:
      result := adx * ady = 2;
    Fou:
      result := (dx <> 0) and (adx = ady);
    Dame:
      result := Possible(Tour, Ax1, Ay1, Ax2, Ay2) or Possible(Fou, Ax1, Ay1, Ax2, Ay2);
    Roi:
      result := (adx + ady <= 2) and ((adx = 1) or (ady = 1));
  end;
end;

function Cibles(const APiece: TPiece; const AIdx: integer): TDamier;
var
  x1, y1, x2, y2: integer;
begin
  x1 := AIdx mod 8;
  y1 := AIdx div 8;
  result := 0;
  for y2 := 7 downto 0 do
    for x2 := 0 to 7 do
      if Possible(APiece, x1, y1, x2, y2) then
        Allume(result, FCase(x2, y2));
end;

function Chemin(const ACase1, ACase2: integer): TDamier;
var
  x1, y1, x2, y2, dx, dy: integer;
begin
  result := 0;
  x1 := ACase1 mod 8;
  y1 := ACase1 div 8;
  x2 := ACase2 mod 8;
  y2 := ACase2 div 8;
  dx := x2 - x1;
  dy := y2 - y1;
  if ((dx <> 0) or (dy <> 0))
  and (
    ((dx = 0) or (dy = 0))
    or (Abs(dx) = Abs(dy))
  ) then
  begin
    if dx <> 0 then dx := dx div Abs(dx);
    if dy <> 0 then dy := dy div Abs(dy);
    repeat
      Inc(x1, dx);
      Inc(y1, dy);
      if (x1 <> x2) or (y1 <> y2) then
        result := result or FCase(x1, y1);
    until (x1 = x2) and (y1 = y2);
  end;
end;

end.
