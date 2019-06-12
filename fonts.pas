
unit Fonts;

interface

const
  GKChars = ' pnbrqk/PNBRQK omvtwl/OMVTWL12345789'; // Gary Katch
  AMChars = ' pnbrqk+PNBRQK omvtwl+OMVTWL!"#$%/()'; // Armando Marroquin
  EPDChars = 'PNBRQKpnbrqk';
  HTMLHead =
  '<!DOCTYPE html>'#10 +
  '<html>'#10 +
  '<head>'#10 +
  '<title>Chess position</title>'#10 +
  '</head>'#10 +
  '<body>'#10 +
  '<p style="font-family:chess alfonso-x;color:midnightblue;font-size:32px;">'#10 +
  '<!-- Chess Alfonso-X and other chess fonts by Armando H. Marroquin :'#10 +
  'http://www.enpassant.dk/chess/fonteng.htm -->'#10;
  HTMLFoot =
  '</p>'#10 +
  '</body>'#10 +
  '</html>';
  
type
  TQuid = (nothing, pawn, knight, bishop, rook, queen, king);
  TColor = (none, white, black);
  TPieceData = record
    quid: TQuid;
    color: TColor;
  end;
  TPiecePlacement = array[1..8, 1..8] of TPieceData;

function EmptyPosition: TPiecePlacement;
function StandardPosition: TPiecePlacement;
function PositionToHtml(aPosition: TPiecePlacement; aChars: string): string;
function EPDToPosition(aStr: string): TPiecePlacement;
function PositionToEPD(aPosition: TPiecePlacement): string;
function PositionToChar(aPosition: TPiecePlacement; aChars: string; x, y: integer): char;

implementation

function EmptyPosition: TPiecePlacement;
var
  x, y: integer;
begin
  for x := 1 to 8 do
    for y := 1 to 8 do
    begin
      result[x, y].quid := nothing;
      result[x, y].color := none;
    end;
end;

function StandardPosition: TPiecePlacement;
var
  x, y: integer;
begin
  for x := 1 to 8 do
    for y := 3 to 6 do
    begin
      result[x, y].quid := nothing;
      result[x, y].color := none;
    end;
  for x := 1 to 8 do
  begin
    result[x, 2].quid := pawn;
    result[x, 7].quid := pawn;
  end;
  result[1, 1].quid := rook;
  result[1, 8].quid := rook;
  result[2, 1].quid := knight;
  result[2, 8].quid := knight;
  result[3, 1].quid := bishop;
  result[3, 8].quid := bishop;
  result[4, 1].quid := queen;
  result[4, 8].quid := queen;
  result[5, 1].quid := king;
  result[5, 8].quid := king;
  result[6, 1].quid := bishop;
  result[6, 8].quid := bishop;
  result[7, 1].quid := knight;
  result[7, 8].quid := knight;
  result[8, 1].quid := rook;
  result[8, 8].quid := rook;
  for x := 1 to 8 do
  begin
    result[x, 1].color := white;
    result[x, 2].color := white;
    result[x, 7].color := black;
    result[x, 8].color := black;
  end;
end;

function PositionToHtml(aPosition: TPiecePlacement; aChars: string): string;
var
  a: array[0..9, 0..9]of char;
  x, y, z: integer;
begin
  a[0, 9] := aChars[29];
  for x := 1 to 8 do
    a[x, 9] := aChars[30];
  a[9, 9] := aChars[31];
  for y := 1 to 8 do
  begin
    a[0, y] := aChars[32];
    a[9, y] := aChars[33];
  end;
  a[0, 0] := aChars[34];
  for x := 1 to 8 do
    a[x, 0] := aChars[35];
  a[9, 0] := aChars[36];
  
  for x := 1 to 8 do
    for y := 1 to 8 do
    begin
      z := Ord(aPosition[x, y].quid);
      if aPosition[x, y].color = black then
        Inc(z, 14);
      if (x + y) mod 2 = 0 then
        Inc(z, 7);
      a[x, y] := aChars[z + 1];
    end;
  
  //result := HTMLHead;
  result := '';
  for y := 9 downto 0 do
  begin
    for x := 0 to 9 do
      result := result + a[x, y];
    result := result + '<br>'#10; 
  end;
  //result := result + HTMLFoot;
end;

function EPDToPosition(aStr: string): TPiecePlacement;
var
  i, j, x, y: integer;
begin
  result := EmptyPosition;

  i := 1;
  x := 1;
  y := 8;

  while y > 0 do
  begin
  
    if i > Length(aStr) then
      exit;
    if Pos(aStr[i], 'PNBRQKpnbrqk12345678/') = 0 then
      exit;

    if aStr[i] = '/' then
      if x = 1 then
        Inc(i)
      else
        exit;

    j := Pos(aStr[i], '12345678');
    if j > 0 then
    begin
      while j > 0 do
      begin
        Inc(x);
        if x = 9 then
        begin
          x := 1;
          Dec(y);
        end;
        Dec(j);
      end;
      Inc(i);
    end;

    j := Pos(aStr[i], 'PNBRQKpnbrqk');
    if j > 0 then
    begin
      result[x, y].quid := TQuid((j - 1) mod 6 + 1);
      if j < 7 then
        result[x, y].color := white
      else
        result[x, y].color := black;
      Inc(x);
      if x = 9 then
      begin
        x := 1;
        Dec(y);
      end;
      Inc(i);
    end;
    
  end;
end;
  
function PositionToEPD(aPosition: TPiecePlacement): string;
var
  x, y, z: integer;
  c: char;
begin
  result := '';
  for y := 8 downto 1 do
  begin
    x := 1;
    while x < 9 do
    begin
      if (Ord(aPosition[x, y].quid) > Ord(nothing)) then
      begin
        z := Ord(aPosition[x, y].quid);
        if aPosition[x, y].color = black then
          Inc(z, 6);
        c := EPDChars[z];
        Inc(x);
      end else
      begin
        z := 0;
        repeat
          Inc(z);
          Inc(x);
        until (Ord(aPosition[x, y].quid) > Ord(nothing)) or (x = 9);
        c := Chr(z + Ord('0'));
      end;
      result := result + c;
    end;

    if y > 1 then
      result := result + '/';
  end;
end;

function PositionToChar(aPosition: TPiecePlacement; aChars: string; x, y: integer): char;
begin
  if (x = 0)
  or (x = 9)
  or (y = 0)
  or (y = 9) then
  begin
    if (x = 0) and (y = 0) then result := '1';
    if (x > 0) and (x < 9) and (y = 0) then result := '2';
    if (x = 9) and (y = 0) then result := '3';
    if (x = 0) and (y > 0) and (y < 9) then result := '4';
    if (x = 9) and (y > 0) and (y < 9) then result := '5';
    if (x = 0) and (y = 9) then result := '7';
    if (x > 0) and (x < 9) and (y = 9) then result := '8';
    if (x = 9) and (y = 9) then result := '9';
  end else
  begin
    if aPosition[x, 9 - y].color = white then
    begin
      if (x + 9 - y) mod 2 = 1 then
      begin
        case aPosition[x, 9 - y].quid of
          nothing: result := ' ';
          pawn: result := 'p';
          knight: result := 'n';
          bishop: result := 'b';
          rook: result := 'r';
          queen: result := 'q';
          king: result := 'k';
        end;
      end else
      begin
        case aPosition[x, 9 - y].quid of
          nothing: result := '/';
          pawn: result := 'P';
          knight: result := 'N';
          bishop: result := 'B';
          rook: result := 'R';
          queen: result := 'Q';
          king: result := 'K';
        end;
      end;
    end else
    begin
      if (x + 9 - y) mod 2 = 1 then
      begin
        case aPosition[x, 9 - y].quid of
          nothing: result := ' ';
          pawn: result := 'o';
          knight: result := 'm';
          bishop: result := 'v';
          rook: result := 't';
          queen: result := 'w';
          king: result := 'l';
        end;
      end else
      begin
        case aPosition[x, 9 - y].quid of
          nothing: result := '/';
          pawn: result := 'O';
          knight: result := 'M';
          bishop: result := 'V';
          rook: result := 'T';
          queen: result := 'W';
          king: result := 'L';
        end;
      end;
    end;
  end;
end;

end.
