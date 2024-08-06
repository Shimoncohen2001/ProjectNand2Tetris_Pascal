all: JackAnalyzer

JackTokenizer:
	fpc JackTokenizer.pas

SymbolTable:
	fpc SymbolTable.pas

VMWriter:
	fpc VMWriter.pas

CompilationEngine: JackTokenizer SymbolTable VMWriter
	fpc CompilationEngine.pas

JackAnalyzer: JackTokenizer CompilationEngine SymbolTable VMWriter
	fpc JackAnalyzer.pas
	rm *.o *.ppu

clean:
	rm -f JackAnalyzer *.o *.ppu