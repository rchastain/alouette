
{**
@abstract(
Liste d'arbres.
)
}

unit TreeList;

interface

uses
  SysUtils, Classes, Tree;

type
  TTreeList = class
    private
      FList: TList;
    public
      constructor Create;
      destructor Destroy; override;
      function FindTree(const AData: string): integer;
      procedure AddLine(const ALine: TStrings);
      procedure AddLine(const ALine: string);
      function GetTreeCount: integer;
      function LoadFromFile(const AFilename: string): boolean;
      function LoadFromFileCompact(const AFilename: string): boolean;
      function GetTextCompact: string;
      function FindMoveToPlay(const ALine: TStrings; const ARandom: boolean): string;
      function FindMoveToPlay(const ALine: string; const ARandom: boolean = TRUE): string;
      procedure SaveToFile(const AFileName: TFileName);
      procedure SaveToFileCompact(const AFileName: TFileName); 
  end;
  
implementation

const
  CTreeNotFound = -1;
  
constructor TTreeList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TTreeList.Destroy;
var
  LIndex: integer;
begin
  for LIndex := 0 to Pred(FList.Count) do
    TTree(FList[LIndex]).Free;
  FList.Free;
  inherited Destroy;
end;

function TTreeList.FindTree(const AData: string): integer;
var
  LIndex: integer;
begin
  result := CTreeNotFound;
  LIndex := 0;
  while (result = CTreeNotFound) and (LIndex < FList.Count) do
    if TTree(FList.Items[LIndex]).FindNode(nil, AData) <> nil then
      result := LIndex
    else
      Inc(LIndex);
end;

procedure TTreeList.AddLine(const ALine: TStrings);
var
  LTree: TTree;
  LTreeIndex: integer;
begin
  if ALine.Count = 0 then
    Exit;
  LTreeIndex := FindTree(ALine.Strings[0]);
  if LTreeIndex = CTreeNotFound then
  begin
    LTree := TTree.Create;
    FList.Add(LTree);
    LTreeIndex := Pred(FList.Count);
  end;
  TTree(FList.Items[LTreeIndex]).AddLine(ALine);
end;

procedure TTreeList.AddLine(const ALine: string);
var
  LLine: TStringList;
begin
  LLine := TStringList.Create;
  LLine.DelimitedText := ALine;
  AddLine(LLine);
  LLine.Free;
end;

function TTreeList.GetTreeCount: integer;
begin
  result := FList.Count;
end;

function TTreeList.LoadFromFile(const AFilename: string): boolean;
var
  LFile, LLine: TStringList;
  i: integer;
begin
  result := TRUE;
  LFile := TStringList.Create;
  LLine := TStringList.Create;
  try
    LFile.LoadFromFile(AFilename);
    for i := 0 to Pred(LFile.Count) do
    begin
      LLine.DelimitedText := LFile[i];
      AddLine(LLine);
    end;
  except
    result := FALSE;
  end;
  LFile.Free;
  LLine.Free;
end;

function TTreeList.LoadFromFileCompact(const AFilename: string): boolean;
const
  CMaxMoves = 100;
var
  LMoves: array[1..CMaxMoves] of string;
  LIndex: integer;
  LDone: boolean;
  LList: TStringList;
procedure Store(const AIndex: integer; const AMove: string);
var
  i: integer;
begin
  if AIndex > LIndex then
  begin
    LMoves[AIndex] := AMove;
    LDone := FALSE;
  end;
  if (AIndex < LIndex) and not LDone then
  begin
    for i := 1 to Succ(AIndex) do
       LList.Append(LMoves[i]);
    AddLine(LList);
    LList.Clear;
    LDone := TRUE;
  end;
  LIndex := AIndex;
end;
procedure Process(const AStr: string);
var
  LLen, LLevel, LPos: integer;
  LData: string;
  c: char;
begin
  LLevel := 0;
  LPos := 1;
  LData := '';
  LLen := Length(AStr);
  while LPos <= LLen do
  begin
    c := AStr[LPos];
    if c = '(' then
    begin
      Store(LLevel, LData);
      Inc(LLevel);
      LData := ''
    end else
    if c = ')' then
    begin
      Store(LLevel, LData);
      Dec(LLevel);
      LData := '';
    end else
      LData := LData + c;
    Inc(LPos);
  end;
  Store(0, 'Au revoir !');
end;
var
  LFile: TStringList;
begin
  result := TRUE;
  LFile := TStringList.Create;
  try
    LFile.LoadFromFile(AFilename);
    if LFile.Count >= 1 then
    begin
      LIndex := 0;
      LDone := FALSE;
      LList := TStringList.Create;
      Process(LFile[0]);
      LList.Free;
    end;
  except
    result := FALSE;
  end;
  LFile.Free;
end;

function TTreeList.GetTextCompact: string;
var
  LTreeIndex: integer;
begin
  result := '';
  for LTreeIndex := 0 to Pred(FList.Count) do
    result := Concat(
      result,
      TTree(FList[LTreeIndex]).GetTextCompact
    );
end;

function TTreeList.FindMoveToPlay(const ALine: TStrings; const ARandom: boolean): string;
var
  LTreeIndex: integer;
begin
  if ALine.Count = 0 then
    if ARandom then
      Exit(TTree(FList[Random(FList.Count)]).GetRoot^.Data)
    else
      Exit(TTree(FList[0]).GetRoot^.Data);
  LTreeIndex := FindTree(ALine.Strings[0]);
  if LTreeIndex = CTreeNotFound then
    result := ''
  else
    result := TTree(FList[LTreeIndex]).FindMoveToPlay(ALine, ARandom);
end;

function TTreeList.FindMoveToPlay(const ALine: string; const ARandom: boolean): string;
var
  LLine: TStringList;
begin
  LLine := TStringList.Create;
  LLine.DelimitedText := ALine;
  result := FindMoveToPlay(LLine, ARandom);
  LLine.Free;
end;

procedure TTreeList.SaveToFile(const AFileName: TFileName); 
var
  LFile: TStringList;
  LIndex: integer;
begin
  LFile := TStringList.Create;
  for LIndex := 0 to Pred(FList.Count) do
    TTree(FList[LIndex]).SaveToFile(LFile);
  LFile.SaveToFile(AFileName);
  LFile.Free;
end;

procedure TTreeList.SaveToFileCompact(const AFileName: TFileName); 
var
  LLine: string;
  LFile: TStringList;
begin
  LFile := TStringList.Create;
  LLine := GetTextCompact;
  LFile.Append(LLine);
  LFile.SaveToFile(AFileName);
  LFile.Free;
end;

end.
