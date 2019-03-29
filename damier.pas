
{**
  @abstract(Damier binaire.)
  Représentation d'un damier dans un nombre entier à 64 chiffres binaires.
  Chaque chiffre binaire représente une case du damier.
}

unit Damier;

interface

type
  {** Le damier est représenté par un nombre entier à 64 chiffres binaires. }
  TDamier = int64;
  TPiece = (PionBlancPrise, PionNoirPrise, Tour, Cavalier, Fou, Dame, Roi);

function FCase(const ACase: integer): TDamier; overload;
{ Case binaire pour un nombre de 0 à 63. }
function FCase(const Ax, Ay: integer): TDamier; overload;
{ Case binaire pour deux nombres de 0 à 7. }
function FIndex(const Ax, Ay: integer): integer;
function NomCase(const Ax, Ay: integer; const AMajuscule: boolean = FALSE): string; overload;
function NomCase(const ACase: integer; const AMajuscule: boolean = FALSE): string; overload;
function NomCoup(const ADepart, AArrivee: integer): string; overload;
function EncodeCoup(const i, j: integer): integer;
procedure DecodeCoup(const ACoup: integer; out ADepart, AArrivee: integer);
function NomCoup(const ACoup: integer): string; overload;
function DecodeNomCase(const ANom: string): integer;
{ Accepte une chaîne de la forme "e2e4". Renvoie un nombre de 0 à 63. }
function Allumee(const ADamier, ACase: TDamier): boolean;
{ Pour savoir si une case est allumée dans un damier. }
procedure Allume(var ADamier: TDamier; const ACase: TDamier);
{ Allume une case dans un damier. }
procedure Eteint(var ADamier: TDamier; const ACase: TDamier);
{ Éteint une case dans un damier. }
procedure Deplace(var AType, ACouleur: TDamier; const ACaseDep, ACaseArr: TDamier);
{ Éteint une case et en allume une autre. }
function Chaine(const ADamier: TDamier): string;
{ Chaîne de chiffres binaires. }
procedure Affiche(const ADamier: TDamier);
function Affiche_(const ADamier: TDamier): string;
{ Affichage de chiffres binaires en forme d'échiquier. }
function Possible(const APiece: TPiece; const Ax1, Ay1, Ax2, Ay2: integer): boolean;
function Cibles(const APiece: TPiece; const ACase: integer): TDamier;
{ Toutes les cases que la pièce, selon son type, peut atteindre. }
function Chemin(const ACase1, ACase2: integer): TDamier;
{ Les cases à survoler pour aller d'un endroit à un autre. }

const
  CDamierVide = 0;
  
  {** Numérotation des cases, de 0 à 63. }
  A1 = 00; B1 = 01; C1 = 02; D1 = 03; E1 = 04; F1 = 05; G1 = 06; H1 = 07;
  A2 = 08; B2 = 09; C2 = 10; D2 = 11; E2 = 12; F2 = 13; G2 = 14; H2 = 15;
  A3 = 16; B3 = 17; C3 = 18; D3 = 19; E3 = 20; F3 = 21; G3 = 22; H3 = 23;
  A4 = 24; B4 = 25; C4 = 26; D4 = 27; E4 = 28; F4 = 29; G4 = 30; H4 = 31;
  A5 = 32; B5 = 33; C5 = 34; D5 = 35; E5 = 36; F5 = 37; G5 = 38; H5 = 39;
  A6 = 40; B6 = 41; C6 = 42; D6 = 43; E6 = 44; F6 = 45; G6 = 46; H6 = 47;
  A7 = 48; B7 = 49; C7 = 50; D7 = 51; E7 = 52; F7 = 53; G7 = 54; H7 = 55;
  A8 = 56; B8 = 57; C8 = 58; D8 = 59; E8 = 60; F8 = 61; G8 = 62; H8 = 63;
  
  CNomCase: array [0..63] of string[2] = (
    'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
    'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
    'A3', 'B3', 'C3', 'D3', 'E3', 'F3', 'G3', 'H3',
    'A4', 'B4', 'C4', 'D4', 'E4', 'F4', 'G4', 'H4',
    'A5', 'B5', 'C5', 'D5', 'E5', 'F5', 'G5', 'H5',
    'A6', 'B6', 'C6', 'D6', 'E6', 'F6', 'G6', 'H6',
    'A7', 'B7', 'C7', 'D7', 'E7', 'F7', 'G7', 'H7',
    'A8', 'B8', 'C8', 'D8', 'E8', 'F8', 'G8', 'H8'
  );

implementation

uses
  SysUtils;
  
function FCase(const ACase: integer): TDamier;
begin
  Assert((ACase >= 0) and (ACase <= 63));
  result := TDamier(1) shl ACase;
end;

function FCase(const Ax, Ay: integer): TDamier;
begin
  Assert((Ax >= 0) and (Ax <= 7) and (Ay >= 0) and ( Ay <= 7));
  result := FCase(8 * Ay + Ax);
end;

function FIndex(const Ax, Ay: integer): integer;
begin
  result := 8 * Ay + Ax;
end;

function NomCase(const Ax, Ay: integer; const AMajuscule: boolean): string;
begin
  Assert((Ax >= 0) and (Ax <= 7) and (Ay >= 0) and ( Ay <= 7));
  result := Concat(
    Chr(Ax + Ord('a') + (Ord('A') - Ord('a')) * Ord(AMajuscule)),
    Chr(Ay + Ord('1'))
  );
end;

function NomCase(const ACase: integer; const AMajuscule: boolean): string;
begin
  result := NomCase(ACase mod 8, ACase div 8, AMajuscule);
end;

function NomCoup(const ADepart, AArrivee: integer): string;
begin
  result := Concat(NomCase(ADepart), NomCase(AArrivee));
end;

function EncodeCoup(const i, j: integer): integer;
begin
  result := 100 * i + j;
end;

procedure DecodeCoup(const ACoup: integer; out ADepart, AArrivee: integer);
begin
  ADepart := ACoup div 100;
  AArrivee := ACoup mod 100;
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

function Allumee(const ADamier, ACase: TDamier): boolean;
begin
  Assert(ACase <> 0);
  result := (ADamier and ACase) = ACase;
end;

procedure Allume(var ADamier: TDamier; const ACase: TDamier);
begin
  ADamier := ADamier or ACase;
end;

procedure Eteint(var ADamier: TDamier; const ACase: TDamier);
begin
  ADamier := ADamier and not ACase;
end;

procedure Deplace(var AType, ACouleur: TDamier; const ACaseDep, ACaseArr: TDamier);
begin
  AType := AType and not ACaseDep or ACaseArr;
  ACouleur := ACouleur and not ACaseDep or ACaseArr;
end;

function Chaine(const ADamier: TDamier): string;
const
  CCaractere: array[boolean] of char = ('0', '1');
var
  i: integer;
begin
  SetLength(result, 64);
  for i := 63 downto 0 do
    result[64 - i] := CCaractere[Allumee(ADamier, FCase(i))];
end;

procedure Affiche(const ADamier: TDamier);
var
  x, y: integer;
  LDamier: string;
begin
  LDamier := Chaine(ADamier);
  WriteLn('+   abcdefgh   +'#13#10);
  for y := 7 downto 0 do
  begin
    Write(Succ(y), '   ');
    for x := 0 to 7 do
      Write(LDamier[64 - 8 * y - x]);
    WriteLn('   ', Succ(y));
  end;
  WriteLn(#13#10'+   abcdefgh   +');
end;

function Affiche_(const ADamier: TDamier): string;
var
  x, y: integer;
  LDamier: string;
begin
  LDamier := Chaine(ADamier);
  result := '+   abcdefgh   +'#13#10#13#10;
  for y := 7 downto 0 do
  begin
    result := concat(result, IntToStr(Succ(y)), '   ');
    for x := 0 to 7 do
      result := concat(result, LDamier[64 - 8 * y - x]);
    result := concat(result, '   ', IntToStr(Succ(y)), #13#10);
  end;
  result := concat(result, #13#10'+   abcdefgh   +');
end;

function Possible(const APiece: TPiece; const Ax1, Ay1, Ax2, Ay2: integer): boolean;
var
  dx, dy: integer;
begin
  case APiece of
    PionBlancPrise: result := ((Ay2 - Ay1) = +1)
                      and (Abs(Ax2 - Ax1) = 1);
    PionNoirPrise:  result := ((Ay2 - Ay1) = -1)
                      and (Abs(Ax2 - Ax1) = 1);
    Tour:           result := (Ax2 = Ax1) xor (Ay2 = Ay1);
    Cavalier:       result := Abs(Ax2 - Ax1) * Abs(Ay2 - Ay1) = 2;
    Fou:            result := (Ax2 <> Ax1)
                      and (Abs(Ax2 - Ax1) = Abs(Ay2 - Ay1));
    Dame:           result := Possible(Tour, Ax1, Ay1, Ax2, Ay2)
                      or Possible(Fou, Ax1, Ay1, Ax2, Ay2);
    Roi:            begin
                      dx := Abs(Ax2 - Ax1);
                      dy := Abs(Ay2 - Ay1);
                      result := (dx + dy <= 2)
                        and ((dx = 1) or (dy = 1));
                    end;
  end;
end;

function Cibles(const APiece: TPiece; const ACase: integer): TDamier;
var
  x1, y1, x2, y2: integer;
begin
  x1 := ACase mod 8;
  y1 := ACase div 8;
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
