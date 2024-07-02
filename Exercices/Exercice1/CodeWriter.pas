
{$mode objfpc}
//shimon final
unit CodeWriter;

interface

uses
  SysUtils, Classes,Parser;

type
  TCommandType =Parser.TCommandType;

  TCodeWriter = class
  private
    FOutputFile: TextFile;
    FFileName: string;
    FLabelCount: Integer;
    procedure IncrementStackPointer;
    procedure DecrementStackPointer;
    procedure PopStackToD;
    procedure PushDToStack;
    procedure LoadStackPointerToA;
    procedure WriteCompareLogic(JumpCommand: string);
    procedure LoadSegment(Segment: string; Index: Integer);
  public
    constructor Create(AFileName: string);
    procedure SetFileName(AFileName: string);
    procedure WriteArithmetic(Command: string);
    procedure WritePushPop(CommandType: TCommandType; Segment: string; Index: Integer);
    procedure WriteLabel(LabelName: string);
    procedure WriteGoto(LabelName: string);
    procedure WriteIf(LabelName: string);
    procedure WriteInit;
    procedure WriteReturn;
    procedure WriteFunction(FunctionName: string; NumLocals: Integer);
    procedure WriteCall(FunctionName: string; NumArgs: Integer);
    procedure Close;
  end;

implementation

constructor TCodeWriter.Create(AFileName: string);
begin
  FFileName := AFileName;
  AssignFile(FOutputFile, ChangeFileExt(FFileName, '.asm'));
  Rewrite(FOutputFile);
  FLabelCount := 0;
end;

procedure TCodeWriter.SetFileName(AFileName: string);
begin
  FFileName := AFileName;
end;

procedure TCodeWriter.WriteArithmetic(Command: string);
begin
  WriteLn(FOutputFile, '// ', Command);
  if Command = 'add' then
  begin
    PopStackToD;
    DecrementStackPointer;
    LoadStackPointerToA;
    WriteLn(FOutputFile, 'M=D+M');
    IncrementStackPointer;
  end
  else if Command = 'sub' then
  begin
    PopStackToD;
    DecrementStackPointer;
    LoadStackPointerToA;
    WriteLn(FOutputFile, 'M=M-D');
    IncrementStackPointer;
  end
  else if Command = 'neg' then
  begin
    DecrementStackPointer;
    LoadStackPointerToA;
    WriteLn(FOutputFile, 'M=-M');
    IncrementStackPointer;
  end
  else if Command = 'eq' then
    WriteCompareLogic('JEQ')
  else if Command = 'gt' then
    WriteCompareLogic('JGT')
  else if Command = 'lt' then
    WriteCompareLogic('JLT')
  else if Command = 'and' then
  begin
    PopStackToD;
    DecrementStackPointer;
    LoadStackPointerToA;
    WriteLn(FOutputFile, 'M=D&M');
    IncrementStackPointer;
  end
  else if Command = 'or' then
  begin
    PopStackToD;
    DecrementStackPointer;
    LoadStackPointerToA;
    WriteLn(FOutputFile, 'M=D|M');
    IncrementStackPointer;
  end
  else if Command = 'not' then
  begin
    DecrementStackPointer;
    LoadStackPointerToA;
    WriteLn(FOutputFile, 'M=!M');
    IncrementStackPointer;
  end;
end;

// procedure for handle label commands
procedure TCodeWriter.WriteLabel(LabelName: string);
begin
  WriteLn(FOutputFile, '(' + LabelName + ')');
end;


// procedure for handle goto commands
procedure TCodeWriter.WriteGoto(LabelName: string);
begin
  WriteLn(FOutputFile, '@' + LabelName);
  WriteLn(FOutputFile, '0;JMP');
end;

// procedure for handle if commands
procedure TCodeWriter.WriteIf(LabelName: string);
begin
  PopStackToD;
  WriteLn(FOutputFile, '@' + LabelName);
  WriteLn(FOutputFile, 'D;JNE');
end;

// sys.init function cette function nous permet d'initilaliser les segments a partir du test fibonaccielement dont le scrypt n'initialise pas cette fois la pile 
// cette fonction initialise en quelque sorte notre virtuelle machine avant son utilisation
//les autres test plus simples que fibbonaccielement s'occuper dans le scrpit d'initialiser les differentes valeurs

procedure TCodeWriter.WriteInit;
begin
  // SP=256
  WriteLn(FOutputFile, '@256');
  WriteLn(FOutputFile, 'D=A');
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'M=D');

  // Call Sys.init
  WriteCall('Sys.init', 0);
end;

// procedure to handle return command
procedure TCodeWriter.WriteReturn;
begin
  // Sauvegarder LCL dans R13
  WriteLn(FOutputFile, '@LCL');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@R13');
  WriteLn(FOutputFile, 'M=D');

  // Retrouver l'adresse de retour dans R14
  WriteLn(FOutputFile, '@5');
  WriteLn(FOutputFile, 'A=D-A');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@R14');
  WriteLn(FOutputFile, 'M=D');

  // Restaurer la valeur de retour à partir de la pile
  DecrementStackPointer;
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@ARG');
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, 'M=D');

  // SP = ARG + 1
  WriteLn(FOutputFile, '@ARG');
  WriteLn(FOutputFile, 'D=M+1');
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'M=D');

  // Restaurer THAT, THIS, ARG, LCL à partir de R13
  WriteLn(FOutputFile, '@R13');
  WriteLn(FOutputFile, 'AM=M-1');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@THAT');
  WriteLn(FOutputFile, 'M=D');

  WriteLn(FOutputFile, '@R13');
  WriteLn(FOutputFile, 'AM=M-1');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@THIS');
  WriteLn(FOutputFile, 'M=D');

  WriteLn(FOutputFile, '@R13');
  WriteLn(FOutputFile, 'AM=M-1');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@ARG');
  WriteLn(FOutputFile, 'M=D');

  WriteLn(FOutputFile, '@R13');
  WriteLn(FOutputFile, 'AM=M-1');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@LCL');
  WriteLn(FOutputFile, 'M=D');

  // Sauter à l'adresse de retour
  WriteLn(FOutputFile, '@R14');
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, '0;JMP');
end;

procedure TCodeWriter.WriteFunction(FunctionName: string; NumLocals: Integer);
var
  i: Integer;
begin
  // Déclare la fonction
  WriteLn(FOutputFile, '(' + FunctionName + ')');

  // Initialiser les variables locales à 0
  for i := 1 to NumLocals do
  begin
    WriteLn(FOutputFile, '@0');
    WriteLn(FOutputFile, 'D=A');
    WriteLn(FOutputFile, '@SP');
    WriteLn(FOutputFile, 'A=M');
    WriteLn(FOutputFile, 'M=D');
    IncrementStackPointer;
  end;
end;

//procedure to handle call comands
procedure TCodeWriter.WriteCall(FunctionName: string; NumArgs: Integer);
var
  ReturnLabel: string;
begin
  // Générer une étiquette de retour unique
  ReturnLabel := FunctionName + '$ret.' + IntToStr(FLabelCount);
  Inc(FLabelCount);

  // Pousser l'étiquette de retour sur la pile
  WriteLn(FOutputFile, '@' + ReturnLabel);
  WriteLn(FOutputFile, 'D=A');
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, 'M=D');
  IncrementStackPointer;

  // Pousser LCL sur la pile
  WriteLn(FOutputFile, '@LCL');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, 'M=D');
  IncrementStackPointer;

  // Pousser ARG sur la pile
  WriteLn(FOutputFile, '@ARG');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, 'M=D');
  IncrementStackPointer;

  // Pousser THIS sur la pile
  WriteLn(FOutputFile, '@THIS');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, 'M=D');
  IncrementStackPointer;

  // Pousser THAT sur la pile
  WriteLn(FOutputFile, '@THAT');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'A=M');
  WriteLn(FOutputFile, 'M=D');
  IncrementStackPointer;

  // Réinitialiser ARG pour le callee
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@5');
  WriteLn(FOutputFile, 'D=D-A');
  WriteLn(FOutputFile, '@' + IntToStr(NumArgs));
  WriteLn(FOutputFile, 'D=D-A');
  WriteLn(FOutputFile, '@ARG');
  WriteLn(FOutputFile, 'M=D');

  // Réinitialiser LCL pour le callee
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@LCL');
  WriteLn(FOutputFile, 'M=D');

  // Sauter à la fonction
  WriteLn(FOutputFile, '@' + FunctionName);
  WriteLn(FOutputFile, '0;JMP');

  // Étiquette de retour
  WriteLn(FOutputFile, '(' + ReturnLabel + ')');
end;

procedure TCodeWriter.WritePushPop(CommandType: TCommandType; Segment: string; Index: Integer);
begin
  if CommandType = C_PUSH then
  begin
    WriteLn(FOutputFile, '// push ', Segment, ' ', Index);
    if Segment = 'constant' then
    begin
      WriteLn(FOutputFile, '@', Index);
      WriteLn(FOutputFile, 'D=A');
    end
    else if Segment = 'local' then
    begin
      LoadSegment('LCL', Index);
      WriteLn(FOutputFile, 'D=M');
    end
    else if Segment = 'argument' then
    begin
      LoadSegment('ARG', Index);
      WriteLn(FOutputFile, 'D=M');
    end
    else if Segment = 'this' then
    begin
      LoadSegment('THIS', Index);
      WriteLn(FOutputFile, 'D=M');
    end
    else if Segment = 'that' then
    begin
      LoadSegment('THAT', Index);
      WriteLn(FOutputFile, 'D=M');
    end
    else if Segment = 'pointer' then
    begin
      WriteLn(FOutputFile, '@R', 3 + Index);
      WriteLn(FOutputFile, 'D=M');
    end
    else if Segment = 'temp' then
    begin
      WriteLn(FOutputFile, '@R', 5 + Index);
      WriteLn(FOutputFile, 'D=M');
    end
    else if Segment = 'static' then
    begin
      WriteLn(FOutputFile, '@', ChangeFileExt(ExtractFileName(FFileName), ''), Index);
      WriteLn(FOutputFile, 'D=M');
    end;
    PushDToStack;
  end
  else if CommandType = C_POP then
  begin
    WriteLn(FOutputFile, '// pop ', Segment, ' ', Index);
    if Segment = 'constant' then
    begin
      WriteLn(FOutputFile, '@', Index);
    end
    else if Segment = 'local' then
    begin
      LoadSegment('LCL', Index);
    end
    else if Segment = 'argument' then
    begin
      LoadSegment('ARG', Index);
    end
    else if Segment = 'this' then
    begin
      LoadSegment('THIS', Index);
    end
    else if Segment = 'that' then
    begin
      LoadSegment('THAT', Index);
    end
    else if Segment = 'pointer' then
    begin
      WriteLn(FOutputFile, '@R', 3 + Index);
    end
    else if Segment = 'temp' then
    begin
      WriteLn(FOutputFile, '@R', 5 + Index);
    end
    else if Segment = 'static' then
    begin
      WriteLn(FOutputFile, '@', ChangeFileExt(ExtractFileName(FFileName), ''), Index);
    end;
    WriteLn(FOutputFile, 'D=A');
    WriteLn(FOutputFile, '@R13');
    WriteLn(FOutputFile, 'M=D');
    PopStackToD;
    WriteLn(FOutputFile, '@R13');
    WriteLn(FOutputFile, 'A=M');
    WriteLn(FOutputFile, 'M=D');
  end;
end;

procedure TCodeWriter.Close;
begin
  CloseFile(FOutputFile);
end;

procedure TCodeWriter.IncrementStackPointer;
begin
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'M=M+1');
end;

procedure TCodeWriter.DecrementStackPointer;
begin
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'M=M-1');
end;

//modified

procedure TCodeWriter.PopStackToD;
begin
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'AM=M-1');
  WriteLn(FOutputFile, 'D=M');
end;


procedure TCodeWriter.PushDToStack;
begin
  LoadStackPointerToA;
  WriteLn(FOutputFile, 'M=D');
  IncrementStackPointer;
end;

procedure TCodeWriter.LoadStackPointerToA;
begin
  WriteLn(FOutputFile, '@SP');
  WriteLn(FOutputFile, 'A=M');
end;
// eq,lt,gt
procedure TCodeWriter.WriteCompareLogic(JumpCommand: string);
begin
  PopStackToD;
  DecrementStackPointer;
  LoadStackPointerToA;
  WriteLn(FOutputFile, 'D=M-D');
  WriteLn(FOutputFile, '@LABEL', FLabelCount);
  WriteLn(FOutputFile, 'D;', JumpCommand);  
  LoadStackPointerToA;
  WriteLn(FOutputFile, 'M=0');
  WriteLn(FOutputFile, '@ENDLABEL', FLabelCount);
  WriteLn(FOutputFile, '0;JMP');  
  WriteLn(FOutputFile, '(LABEL', FLabelCount, ')');
  LoadStackPointerToA;
  WriteLn(FOutputFile, 'M=-1');
  WriteLn(FOutputFile, '(ENDLABEL', FLabelCount, ')');
  IncrementStackPointer;
  Inc(FLabelCount);
end;

procedure TCodeWriter.LoadSegment(Segment: string; Index: Integer);
begin
  WriteLn(FOutputFile, '@', Segment);
  WriteLn(FOutputFile, 'D=M');
  WriteLn(FOutputFile, '@', Index);
  WriteLn(FOutputFile, 'A=D+A');
end;

end.
