program VMProcessor;

uses
  SysUtils;  // Pour utiliser FindFirst, FindNext et FindClose

var
  InputPath, OutputFileName, FileName, Line: string;
  OutputFile: Text;
  BuyTotal, SellTotal: Double;
  SR: TSearchRec;  // TSearchRec pour stocker les informations de fichier

procedure HandleBuy(ProductName: string; Amount: Integer; Price: Double);
begin
  Writeln(OutputFile, '### BUY ', ProductName, ' ###');
  Writeln(OutputFile, Amount * Price:0:1);
  BuyTotal := BuyTotal + (Amount * Price);
end;

procedure HandleSell(ProductName: string; Amount: Integer; Price: Double);
begin
  Writeln(OutputFile, '$$$ SELL ', ProductName, ' $$$');
  Writeln(OutputFile, Amount * Price:0:1);
  SellTotal := SellTotal + (Amount * Price);
end;

procedure ProcessLine(Line: string);
var
  Words: array[1..4] of string;
  i, WordCount, Amount: Integer;
  Price: Double;
  Word: string;
begin
  WordCount := 0;
  Word := '';
  for i := 1 to Length(Line) do
  begin
    if Line[i] = ' ' then
    begin
      if Word <> '' then
      begin
        Inc(WordCount);
        Words[WordCount] := Word;
        Word := '';
      end;
    end
    else
      Word := Word + Line[i];
  end;
  if Word <> '' then
  begin
    Inc(WordCount);
    Words[WordCount] := Word;
  end;

  if WordCount = 4 then
  begin
    Val(Words[3], Amount);
    Val(Words[4], Price);
    if Words[1] = 'buy' then
      HandleBuy(Words[2], Amount, Price)
    else if Words[1] = 'cell' then
      HandleSell(Words[2], Amount, Price);
  end;
end;

procedure ProcessFile(FileName: string);
var
  InputFile: Text;
begin
  Assign(InputFile, FileName);
  Reset(InputFile);

  Writeln(OutputFile, Copy(FileName, 1, Pos('.', FileName) - 1));

  while not Eof(InputFile) do
  begin
    Readln(InputFile, Line);
    ProcessLine(Line);
  end;

  Close(InputFile);
  Writeln(OutputFile);
end;

begin
  Write('Enter the path and folder name: ');
  Readln(InputPath);

  OutputFileName := ExtractFileName(InputPath) + '.asm';
  Assign(OutputFile, OutputFileName);
  Rewrite(OutputFile);

  BuyTotal := 0;
  SellTotal := 0;

  if FindFirst(InputPath + '\*.vm', faAnyFile, SR) = 0 then
  begin
    repeat
      ProcessFile(InputPath + '\' + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  Writeln(OutputFile, 'TOTAL BUY: ', BuyTotal:0:1);
  Writeln(OutputFile, 'TOTAL SELL: ', SellTotal:0:1);

  Writeln('TOTAL BUY: ', BuyTotal:0:1);
  Writeln('TOTAL SELL: ', SellTotal:0:1);

  Close(OutputFile);
end.
