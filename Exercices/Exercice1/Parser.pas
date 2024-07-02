{$mode objfpc}
//shimon final
unit Parser;

interface

uses
  SysUtils, Classes;

type
  TCommandType = (C_ARITHMETIC, C_PUSH, C_POP, C_LABEL, C_GOTO, C_IF, C_FUNCTION, C_RETURN, C_CALL);
  TStringArray = array of string;

  TParser = class
  private
    FFileName: string;
    FFile: TextFile;
    FCurrentCommand: string;
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
    function HasMoreCommands: Boolean;
    procedure Advance;
    function CommandType: TCommandType;
    function Arg1: string;
    function Arg2: Integer;
    property CurrentCommand: string read FCurrentCommand;
  end;

function SplitString(const AString: string; const ADelimiter: Char): TStringArray;

implementation

function SplitString(const AString: string; const ADelimiter: Char): TStringArray;
var
  StartIndex, EndIndex, Count, I: Integer;
begin
  Count := 1;
  for I := 1 to Length(AString) do
    if AString[I] = ADelimiter then
      Inc(Count);

  SetLength(Result, Count);

  StartIndex := 1;
  I := 0;
  while StartIndex <= Length(AString) do
  begin
    EndIndex := StartIndex;
    while (EndIndex <= Length(AString)) and (AString[EndIndex] <> ADelimiter) do
      Inc(EndIndex);
    Result[I] := Copy(AString, StartIndex, EndIndex - StartIndex);
    Inc(I);
    StartIndex := EndIndex + 1;
  end;
end;

constructor TParser.Create(AFileName: string);
begin
  FFileName := AFileName;
  AssignFile(FFile, FFileName);
  Reset(FFile);
end;

destructor TParser.Destroy;
begin
  CloseFile(FFile);
  inherited;
end;

function TParser.HasMoreCommands: Boolean;
begin
  Result := not Eof(FFile);
end;

procedure TParser.Advance;
var
  CommentIndex: Integer;
begin
  if HasMoreCommands then
  begin
    ReadLn(FFile, FCurrentCommand);

    // Remove comments
    CommentIndex := Pos('//', FCurrentCommand);
    if CommentIndex > 0 then
      FCurrentCommand := Copy(FCurrentCommand, 1, CommentIndex - 1);

    // Trim the command
    FCurrentCommand := Trim(FCurrentCommand);

    // Skip empty lines
    if FCurrentCommand = '' then
      Advance;
  end;
end;

function TParser.CommandType: TCommandType;
var
  Command: string;
begin
  Command := Trim(FCurrentCommand);
  if Command <> '' then
    Command := Copy(Command, 1, Pos(' ', Command + ' ') - 1);

  if Command = 'push' then
    Result := C_PUSH
  else if Command = 'pop' then
    Result := C_POP
  else if Command = 'label' then
    Result := C_LABEL
  else if Command = 'goto' then
    Result := C_GOTO
  else if Command = 'if-goto' then
    Result := C_IF
  else if Command = 'function' then
    Result := C_FUNCTION
  else if Command = 'call' then
    Result := C_CALL
  else if Command = 'return' then
    Result := C_RETURN
  else
    Result := C_ARITHMETIC;
end;


//extrait le premier argument d'une commande VM, selon le type de commande
function TParser.Arg1: string;
var
  Parts: TStringArray;
begin
  if CommandType = C_RETURN then
    raise Exception.Create('C_RETURN does not have Arg1');

  Parts := SplitString(FCurrentCommand, ' ');
  if CommandType = C_ARITHMETIC then
    Result := Parts[0]
  else
    Result := Parts[1];
end;

//extrait le deuxième argument d'une commande VM, mais uniquement pour certains types de commandes (C_PUSH, C_POP, C_FUNCTION, C_CALL)

function TParser.Arg2: Integer;
var
  Parts: TStringArray;
begin
  Parts := SplitString(FCurrentCommand, ' ');
  if (CommandType in [C_PUSH, C_POP, C_FUNCTION, C_CALL]) and (Length(Parts) > 2) then
    Result := StrToInt(Parts[2])
  else
    raise Exception.Create('This command does not have Arg2');
end;

end.
