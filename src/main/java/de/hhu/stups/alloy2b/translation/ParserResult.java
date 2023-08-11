package de.hhu.stups.alloy2b.translation;

import java.util.Objects;
import java.util.StringJoiner;

import edu.mit.csail.sdg.alloy4.ConstList;

public final class ParserResult {
	private final String prologTerm;
	private final ConstList<String> commandNames;
	
	public ParserResult(String prologTerm, ConstList<String> commandNames) {
		this.prologTerm = Objects.requireNonNull(prologTerm, "prologTerm");
		this.commandNames = Objects.requireNonNull(commandNames, "commandNames");
	}
	
	public String getPrologTerm() {
		return this.prologTerm;
	}
	
	public ConstList<String> getCommandNames() {
		return this.commandNames;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || this.getClass() != obj.getClass()) {
			return false;
		}
		ParserResult other = (ParserResult)obj;
		return this.getPrologTerm().equals(other.getPrologTerm())
			&& this.getCommandNames().equals(other.getCommandNames());
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(this.getPrologTerm(), this.getCommandNames());
	}
	
	@Override
	public String toString() {
		return new StringJoiner(", ", ParserResult.class.getSimpleName() + "[", "]")
			.add("prologTerm='" + this.getPrologTerm() + "'")
			.add("commandNames=" + this.getCommandNames())
			.toString();
	}
}
