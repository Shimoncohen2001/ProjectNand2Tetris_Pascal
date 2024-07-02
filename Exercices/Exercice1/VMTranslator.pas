program VMTranslator;
//shimon final
uses
  SysUtils, Classes, Parser, CodeWriter;

var
  CodeWriterInstance: TCodeWriter;

procedure Translate(AFileName: string);
var
  Parser: TParser;
  CmdType: TCommandType;
begin
  Parser := TParser.Create(AFileName);
  CodeWriterInstance.SetFileName(ChangeFileExt(AFileName, '.asm'));
  WriteLn('Entering Translate function with file: ', AFileName);

  CodeWriterInstance.WriteInit;

  while Parser.HasMoreCommands do
  begin
    Parser.Advance;
    CmdType := Parser.CommandType;
    WriteLn('Processing command: ', Parser.CurrentCommand); // Debug: Show current command
    case CmdType of
      C_PUSH, C_POP:
        CodeWriterInstance.WritePushPop(CmdType, Parser.Arg1, Parser.Arg2);
      C_ARITHMETIC:
        CodeWriterInstance.WriteArithmetic(Parser.Arg1);
      C_LABEL:
        CodeWriterInstance.WriteLabel(Parser.Arg1);
      C_GOTO:
        CodeWriterInstance.WriteGoto(Parser.Arg1);
      C_IF:
        CodeWriterInstance.WriteIf(Parser.Arg1);
      C_FUNCTION:
        CodeWriterInstance.WriteFunction(Parser.Arg1, Parser.Arg2);
      C_CALL:
        CodeWriterInstance.WriteCall(Parser.Arg1, Parser.Arg2);
      C_RETURN:
        CodeWriterInstance.WriteReturn;
    end;
  end;

  Parser.Free;
end;

procedure IterateFiles(APath: string);
var
  SearchRec: TSearchRec;
begin
  WriteLn('Entering IterateFiles procedure for path: ', APath);
  if FindFirst(APath + '*.vm', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        WriteLn('Found file: ', SearchRec.Name); // Debug: Show found file
        Translate(APath + SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end
  else
    WriteLn('No .vm files found in the directory.'); // Debug: No files found
end;

var
  InputFileName: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: VMTranslator <input file or directory>');
    Exit;
  end;

  InputFileName := ParamStr(1);
  if DirectoryExists(InputFileName) then
  begin
    WriteLn('Entering main block: Directory mode');
    CodeWriterInstance := TCodeWriter.Create(ChangeFileExt(InputFileName, '.asm'));
    IterateFiles(InputFileName + PathDelim); // Ensure path ends with a delimiter
  end
  else    
  begin
    WriteLn('Entering main block: Single file mode');
    CodeWriterInstance := TCodeWriter.Create(ChangeFileExt(InputFileName, '.asm'));
    Translate(InputFileName);
  end;

  CodeWriterInstance.Close;
  CodeWriterInstance.Free;
end.
