{$mode objfpc}

unit VMWriter;

interface

uses
  SysUtils, Classes;

type
  TVMCommand = (vmAdd, vmSub, vmNeg, vmEq, vmGt, vmLt, vmAnd, vmOr, vmNot);
  TSegment = (sgArgument, sgLocal, sgStatic, sgConstant, sgThis, sgThat, sgPointer, sgTemp);

  TVMWriter = class
  private
    output: TStringList;
    fileName: string; // Renamed from outputFileName to fileName
  public
    constructor Create(const outputFileName: string);
    destructor Destroy; override;
    procedure writePush(segment: TSegment; index: Integer);
    procedure writePop(segment: TSegment; index: Integer);
    procedure writeArithmetic(command: TVMCommand);
    procedure writeLabel(const labelName: string);
    procedure writeGoto(const labelName: string);
    procedure writeIf(const labelName: string);
    procedure writeCall(const functionName: string; numArgs: Integer);
    procedure writeFunction(const functionName: string; numLocals: Integer);
    procedure writeInit(); // New method to initialize the VM
    procedure writeReturn();
    procedure close();
  end;

implementation

constructor TVMWriter.Create(const outputFileName: string);
begin
  fileName := outputFileName;
  output := TStringList.Create;
end;

destructor TVMWriter.Destroy;
begin
  output.Free;
  inherited Destroy;
end;

// Convert enum to string representation of segments
function segmentToString(segment: TSegment): string;
begin
  case segment of
    sgArgument: Result := 'argument';
    sgLocal: Result := 'local';
    sgStatic: Result := 'static';
    sgConstant: Result := 'constant';
    sgThis: Result := 'this';
    sgThat: Result := 'that';
    sgPointer: Result := 'pointer';
    sgTemp: Result := 'temp';
  else
    raise Exception.Create('Invalid Segment');
  end;
end;

// Convert enum to string representation of arithmetic commands
function commandToString(command: TVMCommand): string;
begin
  case command of
    vmAdd: Result := 'add';
    vmSub: Result := 'sub';
    vmNeg: Result := 'neg';
    vmEq: Result := 'eq';
    vmGt: Result := 'gt';
    vmLt: Result := 'lt';
    vmAnd: Result := 'and';
    vmOr: Result := 'or';
    vmNot: Result := 'not';
  else
    raise Exception.Create('Invalid Command');
  end;
end;

// Write push command
procedure TVMWriter.writePush(segment: TSegment; index: Integer);
begin
  output.Add(Format('push %s %d', [segmentToString(segment), index]));
end;

// Write pop command
procedure TVMWriter.writePop(segment: TSegment; index: Integer);
begin
  output.Add(Format('pop %s %d', [segmentToString(segment), index]));
end;

// Write arithmetic command
procedure TVMWriter.writeArithmetic(command: TVMCommand);
begin
  output.Add(commandToString(command));
end;

// Write label command
procedure TVMWriter.writeLabel(const labelName: string);
begin
  output.Add(Format('label %s', [labelName]));
end;

// Write goto command
procedure TVMWriter.writeGoto(const labelName: string);
begin
  output.Add(Format('goto %s', [labelName]));
end;

// Write if-goto command
procedure TVMWriter.writeIf(const labelName: string);
begin
  output.Add(Format('if-goto %s', [labelName]));
end;

// Write call command
procedure TVMWriter.writeCall(const functionName: string; numArgs: Integer);
begin
  output.Add(Format('call %s %d', [functionName, numArgs]));
end;

// Write function command
procedure TVMWriter.writeFunction(const functionName: string; numLocals: Integer);
begin
  output.Add(Format('function %s %d', [functionName, numLocals]));
end;

// Write return command
procedure TVMWriter.writeReturn();
begin
  output.Add('return');
end;

// Write the VM initialization call to Sys.init
procedure TVMWriter.writeInit();
begin
  output.Add('call Sys.init 0');
end;

// Save and close output file
procedure TVMWriter.close();
begin
  output.SaveToFile(fileName);
end;

end.
