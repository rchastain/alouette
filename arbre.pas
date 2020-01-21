
{**
@abstract(
Arbre de chaînes de caractères, pour la création et l'édition d'un livre d'ouvertures destiné à un programme de jeu d'échecs.
Adaptation d'un @html(<a href="https://www.developpez.net/forums/d2034310/autres-langages/pascal/langage/representation-l-arbre-d-livre-d-ouvertures-aux-echecs/#post11310888">code source</a>) de Mathieu Dalbin.
)
}

unit Arbre;

{$DEFINE AUTO_CREATE_ROOT}

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
    {** Valeur du nœud (le coup à jouer). }
    Data: string;
  end;

  TTree = class(TObject)
  private
    Root: PTreeNode;
    FText: string;
    procedure ClearNode(ANode: PTreeNode);
    {** Procédure utilisée pour composer la chaîne de caractères contenant l'arbre entier. }
    procedure WriteNode(ANode: PTreeNode);
    {** Procédure utilisée pour composer la chaîne de caractères contenant l'arbre entier. }
    procedure WriteSiblings(ANode: PTreeNode);
    {** Procédure utilisée pour composer la chaîne de caractères contenant l'arbre entier. }
    procedure WriteSiblingsRecursive(ANode: PTreeNode; const ALevel: integer = 0);
  public
    {** Le nombre de nœuds dans l'arbre, en comptant la racine créée automatiquement. }
    Count: integer;
    {** Contructeur modifié, de sorte qu'une racine est automatiquement créée, avec une chaîne vide comme valeur. }
    constructor Create;
    procedure Free;
    function Add(AParent: PTreeNode; AData: string): PTreeNode;
    function GetNodeByName(AName: string): PTreeNode;
    function GetChildByName(AParent: PTreeNode; AName: string): PTreeNode;
    function GetNextChildByName(AParent: PTreeNode; AName: string): PTreeNode;
    function InsertBefore(ASibling: PTreeNode; AData: string): PTreeNode;
    procedure Clear;
    function GetNodeLevel(ANode: PTreeNode): integer;
    function GetChildCount(ANode: PTreeNode): integer;
    function GetRoot: PTreeNode;
    function GetNext(ANode: PTreeNode): PTreeNode;
    {** Fonction renvoyant une chaîne de caractères contenant l'arbre entier, sous la forme @code((e2e4(e7e5(d2d4))(c7c5))). }
    function GetTextCompact: string;
    {** Fonction renvoyant une chaîne de caractères contenant l'arbre entier, sous la forme d'une table. }
    function GetText: string;
    function FindNode(const AParent: PTreeNode; const AData: string): PTreeNode; overload;
    {** Fonction renvoyant (s'il existe) le nœud correspondant à une ligne de coups, par exemple @code(e2e4 e7e5). }
    function FindNode(const ALine: TStrings): PTreeNode; overload;
    function AddLine(const ALine: TStrings): PTreeNode; overload;
    {** Fonction permettant d'ajouter dans le livre une ligne de coups sous la forme @code(e2e4 e7e5). }
    function AddLine(const ALine: string): PTreeNode; overload;
    function FindMoveToPlay(const ALine: TStrings): string; overload;
    function FindMoveToPlayRandom(const ALine: TStrings): string;
    {** Fonction renvoyant un coup à jouer pour une ligne de coups donnée. Le second paramètre permet de choisir un coup au hasard lorsque plusieurs propositions existent. }
    function FindMoveToPlay(const ALine: string; const ARandom: boolean = FALSE): string; overload;
    {** Remplit l'arbre à partir d'un fichier contenant des lignes complètes. }
    function LoadFromFile(const AFilename: string): boolean;
    {** Remplit l'arbre à partir d'un fichier de la forme @code((e2e4(e7e5(d2d4))(c7c5))). }
    function LoadFromFileCompact(const AFilename: string): boolean;
    {** Enregistre l'arbre dans un fichier contenant des lignes complètes. }
    procedure SaveToFile(const AFileName: TFileName);
    {** Enregistre l'arbre dans un fichier, sous la forme @code((e2e4(e7e5(d2d4))(c7c5))). }
    procedure SaveToFileCompact(const AFileName: TFileName);
  end;

implementation

constructor TTree.Create;
begin
  inherited Create;
  Count := 0;
{$IFDEF AUTO_CREATE_ROOT}
  Root := Add(nil, '');
{$INFO A root will be automatically created, with an empty string as value.}
{$ELSE}
  Root := nil;
{$ENDIF}
end;

procedure TTree.Free;
begin
  Clear;
  inherited Free;
end;

function TTree.Add(AParent: PTreeNode; AData: string): PTreeNode;
var
  LNode: PTreeNode;
begin
  if AParent = nil then
  begin
    New(Root);
    Root^.Parent := nil;
    Root^.Sibling := nil;
    Root^.Child := nil;
    Root^.Data := AData;
    result := Root;
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
  Inc(Count);
end;

function TTree.GetNodeByName(AName: string): PTreeNode;
var
  LNode: PTreeNode;
begin
  LNode := Root;
  while LNode <> nil do
  begin
    if LNode^.Data = AName then
      Break;
    LNode := GetNext(LNode);
  end;
  result := LNode;
end;

function TTree.GetChildByName(AParent: PTreeNode; AName: string): PTreeNode;
var
  LNode: PTreeNode;
begin
  LNode := AParent^.Child;
  while LNode <> nil do
  begin
    if LNode^.Data = AName then
      Break;
    LNode := LNode^.Sibling;
  end;
  result := LNode;
end;

function TTree.GetNextChildByName(AParent: PTreeNode; AName: string): PTreeNode;
var
  LNode: PTreeNode;
begin
  LNode := AParent^.Child;
  while LNode <> nil do
  begin
    if LNode^.Data > AName then
      Break;
    LNode := LNode^.Sibling;
  end;
  result := LNode;
end;

function TTree.InsertBefore(ASibling: PTreeNode; AData: string): PTreeNode;
var
  LNode, LParent: PTreeNode;
begin
  LParent := ASibling^.Parent;
  if LParent = nil then
  begin
    result := nil;
    Exit;
  end else
  begin
    if LParent^.Child = ASibling then
    begin
      New(LNode);
      LNode^.Parent := LParent;
      LNode^.Sibling := LParent^.Child;
      LNode^.Child := nil;
      LNode^.Data := AData;
      LParent^.Child := LNode;
      result := LNode;
    end else
    begin
      New(LNode);
      LNode^.Parent := LParent;
      LNode^.Sibling := ASibling^.Sibling;
      LNode^.Child := nil;
      LNode^.Data := AData;
      ASibling^.Sibling := LNode;
      result := LNode;
    end;
  end;
end;

procedure TTree.ClearNode(ANode: PTreeNode);
begin
  if ANode = nil then
    Exit;
  if ANode^.Sibling <> nil then ClearNode(ANode^.Sibling);
  if ANode^.Child <> nil then ClearNode(ANode^.Child);
  if (ANode^.Sibling = nil) and (ANode^.Child = nil) then
    Dispose(ANode);
end;

procedure TTree.Clear;
begin
  ClearNode(Root);
  Root := nil;
  Count := 0;
end;

function TTree.GetNodeLevel(ANode: PTreeNode): integer;
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

function TTree.GetChildCount(ANode: PTreeNode): integer;
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
  result := Root;
end;

function TTree.GetNext(ANode: PTreeNode): PTreeNode;
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
    //if (ANode^.Child = nil) and (ANode^.Sibling = nil) then
    //begin
      LNode := ANode^.Parent;
      while (LNode <> nil) and (LNode^.Sibling = nil) do
        LNode := LNode^.Parent;
      if LNode = nil then
        result := nil
      else
        result := LNode^.Sibling;
    //end;
  end else
    result := nil;
end;

procedure TTree.WriteNode(ANode: PTreeNode);
begin
  FText := Concat(FText, '(', ANode^.Data);
  if ANode^.Child <> nil then
    WriteSiblings(ANode^.Child);
  FText := Concat(FText, ')');
end;

procedure TTree.WriteSiblings(ANode: PTreeNode);
begin
  WriteNode(ANode);
  while ANode^.Sibling <> nil do
  begin
    WriteNode(ANode^.Sibling);
    ANode := ANode^.Sibling;
  end;
end;

function TTree.GetTextCompact: string;
begin
  FText := '';
  WriteSiblings(GetRoot^.Child);
  result := FText;
end;

procedure TTree.WriteSiblingsRecursive(ANode: PTreeNode; const ALevel: integer);
begin
  while ANode <> nil do
  begin
    FText := Concat(FText, StringOfChar(' ', 2 * ALevel), ANode^.Data, #13#10);
    if ANode^.Child <> nil then
      WriteSiblingsRecursive(ANode^.Child, Succ(ALevel));
    ANode := ANode^.Sibling;
  end;
end;

function TTree.GetText: string;
begin
  FText := '';
  WriteSiblingsRecursive(GetRoot^.Child);
  result := FText;
end;

function TTree.FindNode(const AParent: PTreeNode; const AData: string): PTreeNode;
var
  LNode: PTreeNode;
begin
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
    Exit(Root);
  LLevel := 0;
  LNode := nil;
  LParent := Root;
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
  LParent := Root;
  while LLevel < ALine.Count do
  begin
    LNode := FindNode(LParent, ALine[LLevel]);
    if LNode = nil then
      LNode := Add(LParent, ALine[LLevel]);
    LParent := LNode;
    Inc(LLevel);
  end;
  result := LNode;
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

function TTree.FindMoveToPlay(const ALine: TStrings): string;
var
  LNode: PTreeNode;
begin
  LNode := FindNode(ALine);
  if (LNode <> nil) and (LNode^.Child <> nil) then
    result := LNode^.Child^.Data
  else
    result := '';
end;

function TTree.FindMoveToPlayRandom(const ALine: TStrings): string;
var
  LNode: PTreeNode;
  LMoves: TStringList;
begin
  LNode := FindNode(ALine);
  if (LNode <> nil) and (LNode^.Child <> nil) then
  begin
    LMoves := TStringList.Create;
    LNode := LNode^.Child;
    while LNode <> nil do
    begin
      LMoves.Append(LNode^.Data);
      LNode := LNode^.Sibling;
    end;
    result := LMoves[Random(LMoves.Count)];
    LMoves.Free;
  end else
    result := '';
end;
  
function TTree.FindMoveToPlay(const ALine: string; const ARandom: boolean): string;
var
  LLine: TStringList;
begin
  LLine := TStringList.Create;
  LLine.DelimitedText := ALine;
  if ARandom then
    result := FindMoveToPlayRandom(LLine)
  else
    result := FindMoveToPlay(LLine);
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

procedure TTree.SaveToFile(const AFileName: TFileName); 
var
  LNode: PTreeNode;
  LFile, LLine, LLine2: TStringList;
  LLevel, i: integer;
begin
  LFile := TStringList.Create;
  LLine := TStringList.Create;
  LLine2 := TStringList.Create;
  LLine2.Delimiter := ' ';
{$IFDEF AUTO_CREATE_ROOT}
  LNode := Root^.Child;
{$ELSE}
  LNode := Root;
{$ENDIF}
  repeat
{$IFDEF AUTO_CREATE_ROOT}
    LLevel := Pred(GetNodeLevel(LNode));
{$ELSE}
    LLevel := GetNodeLevel(LNode);
{$ENDIF}
    //WriteLn(LLevel);
    if LLevel > Pred(LLine.Count) then
      LLine.Append(LNode^.Data)
    else
      LLine[LLevel] := LNode^.Data;
    if LNode^.Child = nil then
    begin
      LLine2.Clear;
      for i := 0 to LLevel do
        LLine2.Append(LLine[i]);
      LFile.Append(LLine2.DelimitedText);
    end;
    LNode := GetNext(LNode);
  until LNode = nil;
  LFile.SaveToFile(AFileName);
  LFile.Free;
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

end.
