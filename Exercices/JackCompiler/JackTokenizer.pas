{$mode objfpc}
unit JackTokenizer;

interface

uses
  SysUtils, Classes;

type
  TTokenType = (ttKeyword, ttSymbol, ttIdentifier, ttIntConst, ttStringConst);
  TKeyword = (kwClass, kwConstructor, kwFunction, kwMethod, kwField, kwStatic,
              kwVar, kwInt, kwChar, kwBoolean, kwVoid, kwTrue, kwFalse, kwNull,
              kwThis, kwLet, kwDo, kwIf, kwElse, kwWhile, kwReturn);

  TJackTokenizer = class
  private
    inputFile: TextFile;
    //currentToken: string;// remettre current topken en priv
    tokens: TStringList;
    currentTokenIndex: Integer;
    procedure tokenizeInput;
    function isKeyword(const token: string): Boolean;
  public
    currentToken: string;
    constructor Create(filename: string);
    destructor Destroy; override;
    function hasMoreTokens(): Boolean;
    procedure advance();
    function tokenType(): TTokenType;
    function keyWord(): string;
    function symbol(): Char;
    function identifier(): string;
    function intVal(): Integer;
    function stringVal(): string;
  end;

implementation

const
  Keywords: array[TKeyword] of string = (
    'class', 'constructor', 'function', 'method', 'field', 'static',
    'var', 'int', 'char', 'boolean', 'void', 'true', 'false', 'null',
    'this', 'let', 'do', 'if', 'else', 'while', 'return'
  );

constructor TJackTokenizer.Create(filename: string);
begin
  AssignFile(inputFile, filename);
  Reset(inputFile);
  tokens := TStringList.Create;
  currentTokenIndex := -1;
  tokenizeInput;
end;

destructor TJackTokenizer.Destroy;
begin
  tokens.Free;
  CloseFile(inputFile);
  inherited Destroy;
end;

procedure TJackTokenizer.tokenizeInput;
var
  line: string;
  i: Integer;
  token: string;
  inCommentBlock: Boolean;

  procedure AddToken(const AToken: string);
  begin
    if AToken <> '' then
    begin
      tokens.Add(AToken);
      WriteLn('Token added: ', AToken); // Debug: Show added token
    end;
  end;

begin
  inCommentBlock := False;
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    i := 1;
    while i <= Length(line) do
    begin
      if inCommentBlock then
      begin
        // Check for end of block comment
        if (i < Length(line)) and (line[i] = '*') and (line[i + 1] = '/') then
        begin
          inCommentBlock := False;
          Inc(i, 2); // Skip the '*/'
        end
        else
        begin
          Inc(i); // Continue skipping characters inside block comment
        end;
      end
      else
      begin
        case line[i] of
          ' ', #9, #10, #13:
            Inc(i); // Skip whitespace
          'a'..'z', 'A'..'Z', '_':
            begin
              token := '';
              while (i <= Length(line)) and (line[i] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
              begin
                token := token + line[i];
                Inc(i);
              end;
              AddToken(token);
            end;
          '0'..'9':
            begin
              token := '';
              while (i <= Length(line)) and (line[i] in ['0'..'9']) do
              begin
                token := token + line[i];
                Inc(i);
              end;
              AddToken(token);
            end;
          '"':
            begin
              token := '"';
              Inc(i);
              while (i <= Length(line)) and (line[i] <> '"') do
              begin
                token := token + line[i];
                Inc(i);
              end;
              if (i <= Length(line)) and (line[i] = '"') then
              begin
                token := token + '"';
                Inc(i);
              end;
              AddToken(token);
            end;
          '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~':
            begin
              // Check for start of comments
              if (line[i] = '/') and (i < Length(line)) then
              begin
                case line[i + 1] of
                  '/': Break; // Line comment: skip rest of the line
                  '*':
                    begin
                      inCommentBlock := True; // Block comment
                      Inc(i, 2);
                      Continue;
                    end;
                end;
              end;
              AddToken(line[i]);
              Inc(i);
            end;
          else
            Inc(i); // Handle invalid characters or comments here if needed
        end;
      end;
    end;
  end;
end;

function TJackTokenizer.hasMoreTokens(): Boolean;
begin
  Result := currentTokenIndex < tokens.Count - 1;
end;

procedure TJackTokenizer.advance();
begin
  if hasMoreTokens() then
  begin
    Inc(currentTokenIndex);
    currentToken := tokens[currentTokenIndex];
  end;
end;

function TJackTokenizer.tokenType(): TTokenType;
begin
  if isKeyword(currentToken) then
    Result := ttKeyword
  else if Length(currentToken) = 1 then
  begin
    if currentToken[1] in ['{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~'] then
      Result := ttSymbol
    else if currentToken[1] in ['0'..'9'] then
      Result := ttIntConst
    else
      Result := ttIdentifier;
  end
  else if (Length(currentToken) > 1) and (currentToken[1] = '"') and (currentToken[Length(currentToken)] = '"') then
    Result := ttStringConst
  else if (currentToken[1] in ['0'..'9']) then
    Result := ttIntConst
  else 
    Result:= ttIdentifier;
end;

function TJackTokenizer.keyWord(): string;
var
  i: TKeyword;
begin
  for i := Low(TKeyword) to High(TKeyword) do
  begin
    if Keywords[i] = currentToken then
    begin
      Result := Keywords[i];
      Exit;
    end;
  end;
  writeln('pas le bon keyword!!!!!!');
  writeln(currentToken);
  //raise Exception.Create('Invalid keyword');
end;


function TJackTokenizer.symbol(): Char;
begin
  if Length(currentToken) = 1 then
    Result := currentToken[1]
  else
    raise Exception.Create('Invalid symbol');
end;

function TJackTokenizer.identifier(): string;
begin
  Result := currentToken;
end;

function TJackTokenizer.intVal(): Integer;
begin
  Result := StrToInt(currentToken);
end;

function TJackTokenizer.stringVal(): string;
begin
  Result := Copy(currentToken, 2, Length(currentToken) - 2); // Remove quotes
end;

function TJackTokenizer.isKeyword(const token: string): Boolean;
var
  i: TKeyword;
begin
  Result := False;
  for i := Low(TKeyword) to High(TKeyword) do
  begin
    if Keywords[i] = token then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
