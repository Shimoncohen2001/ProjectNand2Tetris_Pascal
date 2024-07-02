{$mode objfpc}
program JackAnalyzer;

uses
  SysUtils, Classes, JackTokenizer, CompilationEngine;

var
  CompilationEngineInstance: TCompilationEngine;

procedure Analyze(AFileName: string);
var
  Tokenizer: TJackTokenizer;
begin
  WriteLn('Analyzing file: ', AFileName);
  Tokenizer := TJackTokenizer.Create(AFileName);
  CompilationEngineInstance := TCompilationEngine.Create(Tokenizer, ChangeFileExt(AFileName, '.xml'));

  WriteLn('Starting compilation of class in file: ', AFileName);
  CompilationEngineInstance.compileClass;
  WriteLn('Finished compilation of class in file: ', AFileName);

  Tokenizer.Free;
  CompilationEngineInstance.Free;
end;

procedure IterateFiles(APath: string);
var
  SearchRec: TSearchRec;
begin
  WriteLn('Entering IterateFiles procedure for path: ', APath);
  if FindFirst(APath + '*.jack', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        WriteLn('Found file: ', SearchRec.Name); // Debug: Show found file
        Analyze(APath + SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end
  else
    WriteLn('No .jack files found in the directory.'); // Debug: No files found
end;

var
  InputFileName: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: JackAnalyzer <input file or directory>');
    Exit;
  end;

  InputFileName := ParamStr(1);
  if DirectoryExists(InputFileName) then
  begin
    WriteLn('Entering main block: Directory mode');
    IterateFiles(InputFileName + PathDelim); // Ensure path ends with a delimiter
  end
  else    
  begin
    WriteLn('Entering main block: Single file mode');
    Analyze(InputFileName);
  end;
end.
