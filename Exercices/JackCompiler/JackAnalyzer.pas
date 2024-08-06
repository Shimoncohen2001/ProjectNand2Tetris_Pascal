{$mode objfpc}

program JackAnalyzer;

uses
  SysUtils, Classes, JackTokenizer, CompilationEngine, VMWriter;

var
  CompilationEngineInstance: TCompilationEngine;

procedure Analyze(AFileName: string);
var
  Tokenizer: TJackTokenizer;
  OutputFileName: string;
begin
  WriteLn('Analyzing file: ', AFileName);
  
  // Create a new tokenizer for the input file
  Tokenizer := TJackTokenizer.Create(AFileName);
  try
    // Change output file extension to .vm
    OutputFileName := ChangeFileExt(AFileName, '.vm');
    
    // Create a new compilation engine for the tokenizer and output file
    CompilationEngineInstance := TCompilationEngine.Create(Tokenizer, OutputFileName);
    try
      // Write the initialization code to call Sys.init
      //TVMWriter.writeInit(); // Assurez-vous que VMWriter est une propriété ou un champ accessible

      WriteLn('Starting compilation of class in file: ', AFileName);
      CompilationEngineInstance.compileClass;  // Compile the entire class
      WriteLn('Finished compilation of class in file: ', AFileName);
    finally
      CompilationEngineInstance.Free;
    end;
  finally
    Tokenizer.Free;
  end;
end;

procedure IterateFiles(APath: string);
var
  SearchRec: TSearchRec;
begin
  WriteLn('Entering IterateFiles procedure for path: ', APath);
  
  // Find all .jack files in the specified directory
  if FindFirst(APath + '*.jack', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        WriteLn('Found file: ', SearchRec.Name); // Debug: Show found file
        Analyze(APath + SearchRec.Name); // Analyze each .jack file
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
  
  // Check if the input is a directory or a single file
  if DirectoryExists(InputFileName) then
  begin
    WriteLn('Entering main block: Directory mode');
    IterateFiles(InputFileName + PathDelim); // Ensure path ends with a delimiter
  end
  else
  begin
    WriteLn('Entering main block: Single file mode');
    Analyze(InputFileName); // Analyze the single input file
  end;
end.
