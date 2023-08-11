package de.hhu.stups.alloy2b.translation;

/**
 * Convert the abstract syntax tree of an Alloy model to a Prolog term.
 */
public final class Alloy2BParser {
	private final Alloy2BParserInternal parser;
	
	public Alloy2BParser() {
		this.parser = new Alloy2BParserInternal();
	}
	
	public ParserResult parseFromFile(String alloyModelPath) throws Alloy2BParserErr {
		return this.parser.parseFromFile(alloyModelPath);
	}
}
