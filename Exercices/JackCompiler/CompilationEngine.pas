
{$mode objfpc}
unit CompilationEngine;

interface

uses
  JackTokenizer,SysUtils;

type
  TCompilationEngine = class
  private
    tokenizer: TJackTokenizer;
    outputFile: TextFile;
    //symbolTable: TSymbolTable;
    procedure writeLine(const line: string);
    //procedure //expect(tokenType: TTokenType; const value: string);
  public
    constructor Create(t: TJackTokenizer; outputFileName: string);
    destructor Destroy; override;
    procedure compileClass();
    procedure compileClassVarDec();
    procedure compileSubroutine();
    procedure compileSubroutineBody();
    procedure compileParameterList();
    procedure compileVarDec();
    procedure compileStatements();
    procedure compileDo();
    procedure compileLet();
    procedure compileWhile();
    procedure compileReturn();
    procedure compileIf();
    procedure compileExpression();
    procedure compileTerm();
    procedure compileExpressionList();
  end;

implementation

constructor TCompilationEngine.Create(t: TJackTokenizer; outputFileName: string);
begin
  tokenizer := t;
  AssignFile(outputFile, outputFileName);
  Rewrite(outputFile);
  //symbolTable := TSymbolTable.Create;
end;

destructor TCompilationEngine.Destroy;
begin
  CloseFile(outputFile);
  //symbolTable.Free;
  inherited Destroy;
end;

procedure TCompilationEngine.writeLine(const line: string);
begin
  WriteLn(outputFile, line);
end;

//procedure TCompilationEngine.////expect(tokenType: TTokenType; const value: string);
//begin
//  if (tokenizer.TokenType <> tokenType) or (tokenizer.KeyWord <> value) then
//    raise Exception.CreateFmt('////expected %s, but found %s', [value, tokenizer.KeyWord]);
//  tokenizer.advance;
//end;

procedure TCompilationEngine.compileClass();
begin
  writeLine('<class>');
  tokenizer.advance; // 'class'
  writeLine('<keyword> class </keyword>');
  
  tokenizer.advance; // className
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  
  tokenizer.advance; // '{'
  writeLine('<symbol> { </symbol>');
  
  tokenizer.advance; // static |field    ou |constructor |function |method
  while tokenizer.TokenType = ttKeyword do
  begin
    if (tokenizer.KeyWord = 'static') or (tokenizer.KeyWord = 'field') then
      compileClassVarDec
    else
      break;
  end;

  while (tokenizer.TokenType = ttKeyword) and
        ((tokenizer.KeyWord = 'constructor') or (tokenizer.KeyWord = 'function') or (tokenizer.KeyWord = 'method')) do
  begin
    compileSubroutine;
  end;

  writeLine('<symbol> } </symbol>');
  writeLine('</class>');
end;

procedure TCompilationEngine.compileClassVarDec();
begin
  writeLine('<classVarDec>');
  // 'static' | 'field'
  writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
  tokenizer.advance;
  
  // type
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.advance;
  
  // varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.advance;

  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      writeLine('<symbol> , </symbol>');
      tokenizer.advance;
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
      tokenizer.advance;
    end
    else
      break;
  end;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</classVarDec>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileSubroutine();
begin
  writeLine('<subroutineDec>');
  // 'constructor' | 'function' | 'method'
  writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
  
  
  tokenizer.advance;  // 'void' | type
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');


  tokenizer.advance; // subroutineName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance; // symbol '(' 
  writeLine('<symbol> ( </symbol>');

  compileParameterList;
  // j ai bouffer la parenthese fermer
  writeLine('<symbol> ) </symbol>');

  //je viens de rajouter ca;
  //tokenizer.advance;

  compileSubroutineBody;
  writeLine('</subroutineDec>');
end;

procedure TCompilationEngine.compileSubroutineBody();
begin
  writeLine('<subroutineBody>');
  tokenizer.advance; // Symbol '{'
  writeLine('<symbol> { </symbol>');

  tokenizer.advance; // Expecting either 'var' keyword or statement
  while tokenizer.TokenType = ttKeyword do
  begin
    if tokenizer.KeyWord = 'var' then
      compileVarDec
    else
      break;
  end;

  compileStatements;

  writeLine('<symbol> } </symbol>');
  writeLine('</subroutineBody>');
  tokenizer.advance; // Consume the closing '}' symbol
end;


procedure TCompilationEngine.compileParameterList();
begin
  writeLine('<parameterList>');
  tokenizer.advance;// param or symbol ')'
  while tokenizer.TokenType <> ttSymbol do
  begin
    // type
    if tokenizer.TokenType = ttKeyword then
      writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
    else
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
    tokenizer.advance;

    // varName
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
    tokenizer.advance;

    if tokenizer.TokenType = ttSymbol then
    begin
      if tokenizer.symbol = ',' then
      begin
        writeLine('<symbol> , </symbol>');
        tokenizer.advance;
      end
      else
        break;
    end;
  end;
  writeLine('</parameterList>');
end;

procedure TCompilationEngine.compileVarDec();
begin
  writeLine('<varDec>');
  // deja bouffer dans la compilesubroutinebody
  writeLine('<keyword> var </keyword>');


  tokenizer.advance;//type: int, array ...
  if tokenizer.TokenType = ttKeyword then
    writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>')
  else
    writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance; // varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance;// symbol: ',' ';' ...
  while tokenizer.TokenType = ttSymbol do
  begin
    if tokenizer.symbol = ',' then
    begin
      writeLine('<symbol> , </symbol>');

      tokenizer.advance;// on bouffe un autre identifieur
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

      tokenizer.advance;
    end
    else
      break;
  end;
  // ';' bouffer dans le while
  writeLine('<symbol> ; </symbol>');
  writeLine('</varDec>');


  // a verifier
  tokenizer.advance;
end;

procedure TCompilationEngine.compileStatements();
begin
  writeLine('<statements>');
  // le currentoken qui est verifier en dessous a ete bouffer dans Compilesubroutinebody
  while tokenizer.TokenType = ttKeyword do
  begin
    if tokenizer.KeyWord = 'let' then
      compileLet
    else if tokenizer.KeyWord = 'if' then
      compileIf
    else if tokenizer.KeyWord = 'while' then
      compileWhile
    else if tokenizer.KeyWord = 'do' then
      compileDo
    else if tokenizer.KeyWord = 'return' then
      compileReturn
    else
      break;
  end;
  writeLine('</statements>');

  // Log the current token type and value after compileStatements
  writeln('Exiting compileStatements - Current Token Type: ', Ord(tokenizer.TokenType));
  case tokenizer.TokenType of
    ttKeyword: writeln('Current Keyword: ', tokenizer.KeyWord);
    ttSymbol: writeln('Current Symbol: ', tokenizer.Symbol);
    ttIdentifier: writeln('Current Identifier: ', tokenizer.Identifier);
    ttIntConst: writeln('Current Integer Constant: ', tokenizer.IntVal);
    ttStringConst: writeln('Current String Constant: ', tokenizer.StringVal);
  else
    writeln('Unknown Token Type');
  end;
end;

procedure TCompilationEngine.compileDo();
begin
  writeLine('<doStatement>');
  writeLine('<keyword> do </keyword>');
  tokenizer.advance;

  // subroutineCall
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');
  tokenizer.advance;

  //expect(SYMBOL, '(');
  writeLine('<symbol> ( </symbol>');

  compileExpressionList;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</doStatement>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileLet();
begin
  writeLine('<letStatement>');
  // on a bouffer deja le let dans statement;
  writeLine('<keyword> let </keyword>');

  tokenizer.advance;// varName
  writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

  tokenizer.advance;// on bouffe soit un = sois un '['
  if tokenizer.symbol = '[' then
  begin
    writeLine('<symbol> [ </symbol>');

    tokenizer.advance;// on bouffe ce qui nous faut pour compileExpression
    compileExpression;

    //expect(SYMBOL, ']');
    writeLine('<symbol> ] </symbol>');
  end;

  //on a bouffer le = au dessus
  writeLine('<symbol> = </symbol>');
  
  tokenizer.advance;// on bouffe ce qui nous faut pour compileExpression
  compileExpression;

  //on a deja bouffer ; dans Compileexpression
  writeLine('<symbol> ; </symbol>');
  writeLine('</letStatement>');

  tokenizer.advance;// on bouffe le while, }

  // ajout d un log  dans la fonction compilelet;
  writeln('affichage du current token en sortant pour la derniere fois de la fonction CompileLet ',tokenizer.tokenType);
end;

procedure TCompilationEngine.compileWhile();
begin
  writeLine('<whileStatement>');
  writeLine('<keyword> while </keyword>');

  tokenizer.advance;//(

  //on a bouffe une (
  writeLine('<symbol> ( </symbol>');

  tokenizer.advance;
  compileExpression;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  //expect(SYMBOL, '{');
  writeLine('<symbol> { </symbol>');

  compileStatements;

  //expect(SYMBOL, '}');
  writeLine('<symbol> } </symbol>');
  writeLine('</whileStatement>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileReturn();
begin
  writeLine('<returnStatement>');
  writeLine('<keyword> return </keyword>');
  tokenizer.advance;

  if tokenizer.TokenType <> ttSymbol then
    compileExpression;

  //expect(SYMBOL, ';');
  writeLine('<symbol> ; </symbol>');
  writeLine('</returnStatement>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileIf();
begin
  writeLine('<ifStatement>');
  writeLine('<keyword> if </keyword>');
  tokenizer.advance;

  //expect(SYMBOL, '(');
  writeLine('<symbol> ( </symbol>');

  compileExpression;

  //expect(SYMBOL, ')');
  writeLine('<symbol> ) </symbol>');

  //expect(SYMBOL, '{');
  writeLine('<symbol> { </symbol>');

  compileStatements;

  //expect(SYMBOL, '}');
  writeLine('<symbol> } </symbol>');

  if tokenizer.KeyWord = 'else' then
  begin
    tokenizer.advance;
    writeLine('<keyword> else </keyword>');

    //expect(SYMBOL, '{');
    writeLine('<symbol> { </symbol>');

    compileStatements;

    //expect(SYMBOL, '}');
    writeLine('<symbol> } </symbol>');
  end;

  writeLine('</ifStatement>');
  tokenizer.advance;
end;

procedure TCompilationEngine.compileExpression();
begin
  writeLine('<expression>');
  compileTerm;

  while (tokenizer.TokenType = ttSymbol) and (tokenizer.symbol in ['+', '-', '*', '/', '&', '|', '<', '>', '=']) do
  begin
    writeLine('<symbol> ' + tokenizer.symbol + ' </symbol>');
    tokenizer.advance;
    compileTerm;
  end;

  writeLine('</expression>');
end;

procedure TCompilationEngine.compileTerm();
begin
  writeLine('<term>');

  case tokenizer.TokenType of
    ttIntConst:
    begin
      writeLine('<integerConstant> ' + IntToStr(tokenizer.IntVal) + ' </integerConstant>');// on a deja bouffer le 0 dans expression 

      tokenizer.advance;// on bouffe ;
    end;
    ttStringConst:
    begin
      writeLine('<stringConstant> ' + tokenizer.StringVal + ' </stringConstant>');
      
      tokenizer.advance;// on bouffe symbol ')'
    end;
    ttKeyword:
    begin
      writeLine('<keyword> ' + tokenizer.KeyWord + ' </keyword>');
      tokenizer.advance;
    end;
    ttIdentifier:
    begin
      writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

      tokenizer.advance;// on bouffe un sybol ex '.', <
      if tokenizer.TokenType = ttSymbol then
      begin
        if tokenizer.Symbol = '[' then
        begin
          writeLine('<symbol> [ </symbol>');
          tokenizer.advance;
          compileExpression;
          writeLine('<symbol> ] </symbol>');
          tokenizer.advance;
        end
        else if tokenizer.Symbol = '(' then
        begin
          writeLine('<symbol> ( </symbol>');
          tokenizer.advance;
          compileExpressionList;
          writeLine('<symbol> ) </symbol>');
          tokenizer.advance; // Consume closing ')'
        end
        else if tokenizer.Symbol = '.' then
        begin
          writeLine('<symbol> . </symbol>');

          tokenizer.advance;// on bouffe un identifieur ex: readinit ou new
          writeLine('<identifier> ' + tokenizer.Identifier + ' </identifier>');

          tokenizer.advance;// forecement un (
          writeLine('<symbol> ( </symbol>');

          tokenizer.advance;// on bouffe expression "exrpression"
          compileExpressionList;
          writeLine('<symbol> ) </symbol>');// on a bouff la ) avant

          tokenizer.advance; // on bouffe ;
        end;
      end;
    end;
    ttSymbol:
    begin
      if tokenizer.Symbol = '(' then
      begin
        writeLine('<symbol> ( </symbol>');
        tokenizer.advance;
        compileExpression;
        writeLine('<symbol> ) </symbol>');
        tokenizer.advance; // Consume closing ')'
      end
      else if tokenizer.Symbol in ['-', '~'] then
      begin
        writeLine('<symbol> ' + tokenizer.Symbol + ' </symbol>');
        tokenizer.advance;
        compileTerm;
      end;
    end;
  end;

  writeLine('</term>');
end;


procedure TCompilationEngine.compileExpressionList();
begin
  writeLine('<expressionList>');
  if tokenizer.TokenType <> ttSymbol then // Correct condition to check if expression list is not empty
  begin
    compileExpression;
    while tokenizer.Symbol = ',' do
    begin
      writeLine('<symbol> , </symbol>');
      tokenizer.advance; // Consume the comma
      compileExpression;
    end;
  end;
  writeLine('</expressionList>');
end;

end.
