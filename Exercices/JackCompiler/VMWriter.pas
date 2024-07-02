//unit VMWriter;
//
//interface
//
//uses
//  SysUtils, Classes;
//
//type
//  TSegment = (sgArgument, sgLocal, sgStatic, sgConstant, sgThis, sgThat, sgPointer, sgTemp);
//  TCommand = (cmdAdd, cmdSub, cmdNeg, cmdEq, cmdGt, cmdLt, cmdAnd, cmdOr, cmdNot);
//
//  TVMWriter = class
//  private
//    outputFile: TextFile;
//  public
//    constructor Create(filename: string);
//    procedure writePush(segment: TSegment; index: Integer);
//    procedure writePop(segment: TSegment; index: Integer);
//    procedure writeArithmetic(command: TCommand);
//    procedure writeLabel(labelName: string);
//    procedure writeGoto(labelName: string);
//    procedure writeIf(labelName: string);
//    procedure writeCall(name: string; nArgs: Integer);
//    procedure writeFunction(name: string; nLocals: Integer);
//    procedure writeReturn();
//    procedure close();
//  end;
//
//implementation
//
//constructor TVMWriter.Create(filename: string);
//begin
//  AssignFile(outputFile, filename);
//  Rewrite(outputFile);
//end;
//
//procedure TVMWriter.writePush(segment: TSegment; index: Integer);
//begin
//  // Implémenter le code pour écrire une commande push
//end;
//
//procedure TVMWriter.writePop(segment: TSegment; index: Integer);
//begin
//  // Implémenter le code pour écrire une commande pop
//end;
//
//procedure TVMWriter.writeArithmetic(command: TCommand);
//begin
//  // Implémenter le code pour écrire une commande arithmétique
//end;
//
//procedure TVMWriter.writeLabel(labelName: string);
//begin
//  // Implémenter le code pour écrire une étiquette
//end;
//
//procedure TVMWriter.writeGoto(labelName: string);
//begin
//  // Implémenter le code pour écrire une commande goto
//end;
//
//procedure TVMWriter.writeIf(labelName: string);
//begin
//  // Implémenter le code pour écrire une commande if-goto
//end;
//
//procedure TVMWriter.writeCall(name: string; nArgs: Integer);
//begin
//  // Implémenter le code pour écrire une commande call
//end;
//
//procedure TVMWriter.writeFunction(name: string; nLocals: Integer);
//begin
//  // Implémenter le code pour écrire une commande function
//end;
//
//procedure TVMWriter.writeReturn();
//begin
//  // Implémenter le code pour écrire une commande return
//end;
//
//procedure TVMWriter.close();
//begin
//  CloseFile(outputFile);
//end;
//
//end.
//