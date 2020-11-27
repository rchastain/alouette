
{**
@abstract(
Arbre de chaînes de caractères, pour le livre d'ouvertures d'un programme d'échecs.
Adaptation d'un @html(<a href="https://www.developpez.net/forums/d2034310/autres-langages/pascal/langage/representation-l-arbre-d-livre-d-ouvertures-aux-echecs/#post11310888">code source</a>) de Mathieu Dalbin.
)
}

unit Trees;

interface

uses
  SysUtils, Classes;

type
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    {** Nœud précédent. }
    Parent,
    {** Nœud suivant. }
    Child,
    {** Nœud frère. }
    Sibling: PTreeNode;
    {** Valeur du nœud. }
    Data: string;
  end;

  TTree = class(TObject)
  private
    FRoot: PTreeNode;
    FText: string;
    procedure ClearNode(const ANode: PTreeNode);
    {** Procédure utilisée pour composer la chaîne de caractères contenant l'arbre entier. }
    procedure WriteNode(const ANode: PTreeNode);
    {** Procédure utilisée pour composer la chaîne de caractères contenant l'arbre entier. }
    procedure WriteSiblings(const ANode: PTreeNode);
  public
    {** Le nombre de nœuds dans l'arbre. }
    FCount: integer;
    constructor Create;
    destructor Destroy; override;
    function Add(const AParent: PTreeNode; const AData: string): PTreeNode;
    procedure Clear;
    function GetNodeLevel(const ANode: PTreeNode): integer;
    function GetChildCount(const ANode: PTreeNode): integer;
    function GetRoot: PTreeNode;
    function GetNext(const ANode: PTreeNode): PTreeNode;
    {** Fonction renvoyant une chaîne de caractères contenant l'arbre entier, sous la forme @code((e2e4(e7e5(d2d4))(c7c5))). }
    function GetTextCompact: string;
    function FindNode(const AParent: PTreeNode; const AData: string): PTreeNode; overload;
    {** Fonction renvoyant (s'il existe) le nœud correspondant à une ligne de coups, par exemple @code(e2e4 e7e5). }
    function FindNode(const ALine: TStrings): PTreeNode; overload;
    function AddLine(const ALine: TStrings): PTreeNode; overload;
    {** Fonction permettant d'ajouter dans le livre une ligne de coups sous la forme @code(e2e4 e7e5). }
    function AddLine(const ALine: string): PTreeNode; overload;
    function FindMoveToPlay(const ALine: TStrings; const ARandom: boolean): string; overload;
    {** Fonction renvoyant un coup à jouer pour une ligne de coups donnée. }
    function FindMoveToPlay(const ALine: string; const ARandom: boolean): string; overload;
    {** Remplit l'arbre à partir d'un fichier contenant des lignes complètes. }
    function LoadFromFile(const AFilename: string): boolean;
    {** Remplit l'arbre à partir d'un fichier de la forme @code((e2e4(e7e5(d2d4))(c7c5))). }
    function LoadFromFileCompact(const AFilename: string): boolean;
    {** Enregistre l'arbre dans un fichier contenant des lignes complètes. }
    procedure SaveToFile(const {AFileName: TFileName}AList: TStringList);
    {** Enregistre l'arbre dans un fichier, sous la forme @code((e2e4(e7e5(d2d4))(c7c5))). }
    procedure SaveToFileCompact(const AFileName: TFileName);
  end;
  
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

constructor TTree.Create;
begin
  inherited Create;
  FCount := 0;
  FRoot := nil;
end;

destructor TTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TTree.Add(const AParent: PTreeNode; const AData: string): PTreeNode;
var
  LNode: PTreeNode;
begin
  if AParent = nil then
  begin
    New(FRoot);
    FRoot^.Parent := nil;
    FRoot^.Sibling := nil;
    FRoot^.Child := nil;
    FRoot^.Data := AData;
    result := FRoot;
  end else
    if AParent^.Child = nil then
    begin
      New(AParent^.Child);
      LNode := AParent^.Child;
      LNode^.Parent := AParent;
      LNode^.Sibling := nil;
      LNode^.Child := nil;
      LNode^.Data := AData;
      result := LNode;
    end else
    begin
      LNode := AParent^.Child;
      while LNode^.Sibling <> nil do
        LNode := LNode^.Sibling;
      New(LNode^.Sibling);
      LNode := LNode^.Sibling;
      LNode^.Parent := AParent;
      LNode^.Sibling := nil;
      LNode^.Child := nil;
      LNode^.Data := AData;
      result := LNode;
    end;
  Inc(FCount);
end;

procedure TTree.ClearNode(const ANode: PTreeNode);
begin
  if ANode = nil then
    Exit;
  if ANode^.Sibling <> nil then
    ClearNode(ANode^.Sibling);
  if ANode^.Child <> nil then
    ClearNode(ANode^.Child);
  Dispose(ANode);
end;

procedure TTree.Clear;
begin
  ClearNode(FRoot);
  FRoot := nil;
  FCount := 0;
end;

function TTree.GetNodeLevel(const ANode: PTreeNode): integer;
var
  LNode: PTreeNode;
begin
  if ANode = nil then
  begin
    result := -1;
    Exit;
  end;
  result := 0;
  LNode := ANode;
  while LNode^.Parent <> nil do
  begin
    Inc(result);
    LNode := LNode^.Parent;
  end;
end;

function TTree.GetChildCount(const ANode: PTreeNode): integer;
var
  LNode: PTreeNode;
begin
  LNode := ANode^.Child;
  result := 0;
  while LNode <> nil do
  begin
    LNode := LNode^.Sibling;
    Inc(result);
  end;
end;

function TTree.GetRoot: PTreeNode;
begin
  result := FRoot;
end;

function TTree.GetNext(const ANode: PTreeNode): PTreeNode;
var
  LNode: PTreeNode;
begin
  if ANode <> nil then
  begin
    if ANode^.Child <> nil then
    begin
      result := ANode^.Child;
      Exit;
    end;
    if ANode^.Sibling <> nil then
    begin
      result := ANode^.Sibling;
      Exit;
    end;
    LNode := ANode^.Parent;
    while (LNode <> nil) and (LNode^.Sibling = nil) do
      LNode := LNode^.Parent;
    if LNode = nil then
      result := nil
    else
      result := LNode^.Sibling;
  end else
    result := nil;
end;

procedure TTree.WriteNode(const ANode: PTreeNode);
begin
  FText := Concat(FText, '(', ANode^.Data);
  if ANode^.Child <> nil then
    WriteSiblings(ANode^.Child);
  FText := Concat(FText, ')');
end;

procedure TTree.WriteSiblings(const ANode: PTreeNode);
var
  LNode: PTreeNode;
begin
  LNode := ANode;
  WriteNode(LNode);
  while LNode^.Sibling <> nil do
  begin
    WriteNode(LNode^.Sibling);
    LNode := LNode^.Sibling;
  end;
end;

function TTree.GetTextCompact: string;
begin
  FText := '';
  WriteSiblings(GetRoot);
  result := FText;
end;

function TTree.FindNode(const AParent: PTreeNode; const AData: string): PTreeNode;
var
  LNode: PTreeNode;
begin
  if AParent = nil then
    if (FRoot <> nil) and (FRoot^.Data = AData) then
      Exit(FRoot)
    else
      Exit(nil);
  LNode := AParent^.Child;
  while LNode <> nil do
  begin
    if LNode^.Data = AData then
      Break;
    LNode := LNode^.Sibling;
  end;
  result := LNode;
end;

function TTree.FindNode(const ALine: TStrings): PTreeNode;
var
  LLevel: integer;
  LNode, LParent: PTreeNode;
begin
  if ALine.Count = 0 then
    Exit(FRoot);
  LLevel := 0;
  LNode := nil;
  LParent := nil;
  while LLevel < ALine.Count do
  begin
    LNode := FindNode(LParent, ALine[LLevel]);
    if LNode = nil then
      Break;
    LParent := LNode;
    Inc(LLevel);
  end;
  result := LNode;
end;

function TTree.AddLine(const ALine: TStrings): PTreeNode;
var
  LLevel: integer;
  LNode, LParent: PTreeNode;
begin
  LLevel := 0;
  LNode := FRoot;
  if LNode = nil then
    while LLevel < ALine.Count do
    begin
      LNode := Add(LNode, ALine[LLevel]);
      Inc(LLevel);
    end
  else
    if LNode^.Data = ALine[LLevel] then
    begin
      Inc(LLevel);
      LParent := LNode;
      while LLevel < ALine.Count do
      begin
        LNode := FindNode(LParent, ALine[LLevel]);
        if LNode = nil then
          LNode := Add(LParent, ALine[LLevel]);
        LParent := LNode;
        Inc(LLevel);
      end;
    end else
      result := nil;
end;

function TTree.AddLine(const ALine: string): PTreeNode;
var
  LLine: TStringList;
begin
  LLine := TStringList.Create;
  LLine.DelimitedText := ALine;
  result := AddLine(LLine);
  LLine.Free;
end;

function TTree.FindMoveToPlay(const ALine: TStrings; const ARandom: boolean): string;
var
  LNode: PTreeNode;
  LSiblings: TStringList;
begin
  if ALine.Count = 0 then
    if (FRoot <> nil)then
      Exit(FRoot^.Data)
    else
      Exit('');
  LNode := FindNode(ALine);
  if (LNode <> nil) and (LNode^.Child <> nil) then
  begin
    if ARandom then
    begin
      LSiblings := TStringList.Create;
      LNode := LNode^.Child;
      while LNode <> nil do
      begin
        LSiblings.Append(LNode^.Data);
        LNode := LNode^.Sibling;
      end;
      result := LSiblings[Random(LSiblings.Count)];
      LSiblings.Free;
    end else
      result := LNode^.Child^.Data
  end else
    result := '';
end;

function TTree.FindMoveToPlay(const ALine: string; const ARandom: boolean): string;
var
  LLine: TStringList;
begin
  LLine := TStringList.Create;
  LLine.DelimitedText := ALine;
  result := FindMoveToPlay(LLine, ARandom);
  LLine.Free;
end;

function TTree.LoadFromFile(const AFilename: string): boolean;
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

function TTree.LoadFromFileCompact(const AFilename: string): boolean;
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

procedure TTree.SaveToFile(const AList: TStringList); 
var
  LNode: PTreeNode;
  LLine, LLine2: TStringList;
  LLevel, i: integer;
begin
  LLine := TStringList.Create;
  LLine2 := TStringList.Create;
  LLine2.Delimiter := ' ';
  LNode := FRoot;
  repeat
    LLevel := GetNodeLevel(LNode);
    if LLevel > Pred(LLine.Count) then
      LLine.Append(LNode^.Data)
    else
      LLine[LLevel] := LNode^.Data;
    if LNode^.Child = nil then
    begin
      LLine2.Clear;
      for i := 0 to LLevel do
        LLine2.Append(LLine[i]);
      AList.Append(LLine2.DelimitedText);
    end;
    LNode := GetNext(LNode);
  until LNode = nil;
  LLine.Free;
  LLine2.Free;
end;

procedure TTree.SaveToFileCompact(const AFileName: TFileName); 
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
  if FList.Count = 0 then
    Exit('');
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
