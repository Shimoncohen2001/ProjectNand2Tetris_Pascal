all: JackAnalyzer
JackTokenizer:
	fpc JackTokenizer.pas

CompilationEngine: JackTokenizer
	fpc CompilationEngine.pas

JackAnalyzer: JackTokenizer CompilationEngine
	fpc JackAnalyzer.pas
	rm *.o *.ppu
clean:
	rm -f JackAnalyzer *.o *.ppu
