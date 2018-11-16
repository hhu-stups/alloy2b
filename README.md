# A Translation from Alloy to B

[![Build Status](https://travis-ci.org/hhu-stups/alloy2b.svg?branch=master&style=flat-square)](https://travis-ci.org/hhu-stups/alloy2b)

We introduce a translation of the formal specification language Alloy to classical B.
The translation allows us to load Alloy models into [ProB Tcl/Tk](https://www3.hhu.de/stups/prob/index.php/Main_Page/) in order to find solutions to the model's constraints.
Our translation closely follows the Alloy grammar, each construct is translated into a semantically equivalent component of the B language.

This repository does not contain a standalone software but the parser to translate an Alloy model to a Prolog term which can then be processed by ProB.
We use the Alloy parser which can be extracted from the [jar-file](http://alloytools.org/download.html).
The actual translation is implemented in Prolog and located in the core of ProB.
Further information on the translation can be found in our [paper](https://www3.hhu.de/stups/downloads/pdf/Alloy2B.pdf).

[Here](https://www3.hhu.de/stups/prob/index.php/Alloy) you can find some examples how to load Alloy models in ProB Tcl/Tk.


# Note
The tool is still in progress and we do not support the complete Alloy language yet. Please send us any Alloy model that cannot be loaded by ProB and does not use any feature listed below.

Known missing features:
- sequences: afterLastIdx, add, delete, setAt, insert, subseq
- graphs
- traces
- binary interactions between different signatures without a parent type except of UNIV

# Developer Note

Gradle: ```compile 'de.hhu.stups:alloy2b:1.0-SNAPSHOT'```

Load an Alloy model from file and translate it to a Prolog term:

```Alloy2BParser parser = new Alloy2BParser();```

```String prologTerm = parser.alloyToPrologTerm(alloyFilePath);```

The method alloyToPrologTerm() throws an [Err](http://alloy.lcs.mit.edu/alloy/documentation/alloy-api/edu/mit/csail/sdg/alloy4/Err.html) exception if the Alloy model contains parse errors.
The Err class provides a [Pos](http://alloy.lcs.mit.edu/alloy/documentation/alloy-api/edu/mit/csail/sdg/alloy4/Pos.html) object with further information on the error's location.

The ProB2 Prolog interface provides the predicate ```load_alloy_spec_from_term/1``` to translate the Prolog term to B and load the model into ProB.
