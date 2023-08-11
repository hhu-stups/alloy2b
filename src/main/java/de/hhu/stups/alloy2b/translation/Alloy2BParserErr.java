package de.hhu.stups.alloy2b.translation;

import java.util.Objects;

import edu.mit.csail.sdg.alloy4.Err;

@SuppressWarnings("serial")
public final class Alloy2BParserErr extends Exception {
	private final String filename;
	private final int colStart;
	private final int rowStart;
	private final int colEnd;
	private final int rowEnd;
	
	public Alloy2BParserErr(String message, Throwable cause, String filename, int colStart, int rowStart, int colEnd, int rowEnd) {
		super(Objects.requireNonNull(message, "message"), cause);
		this.filename = Objects.requireNonNull(filename, "filename");
		this.colStart = colStart;
		this.rowStart = rowStart;
		this.colEnd = colEnd;
		this.rowEnd = rowEnd;
	}
	
	public Alloy2BParserErr(String message, String filename, int colStart, int rowStart, int colEnd, int rowEnd) {
		this(message, null, filename, colStart, rowStart, colEnd, rowEnd);
	}
	
	public Alloy2BParserErr(Err err) {
		this(err.msg, err, err.pos.filename, err.pos.x, err.pos.y, err.pos.x2, err.pos.y2);
	}
	
	public String getFilename() {
		return this.filename;
	}
	
	public int getColStart() {
		return this.colStart;
	}
	
	public int getRowStart() {
		return this.rowStart;
	}
	
	public int getColEnd() {
		return this.colEnd;
	}
	
	public int getRowEnd() {
		return this.rowEnd;
	}
}
