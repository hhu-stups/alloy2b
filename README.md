# A Translation from Alloy to B

[![Build Status](https://travis-ci.org/hhu-stups/alloy2b.svg?branch=master&style=flat-square)](https://travis-ci.org/hhu-stups/alloy2b)

We introduce a translation of the specification language Alloy to classical B.
The translation allows us to load Alloy models into ProB in order to find solutions to the model's constraints.
Our translation closely follows the Alloy grammar, each construct is translated into a semantically equivalent component of the B language.
In addition to basic Alloy constructs, our approach supports integers and orderings.
The translation is fully automated by the tool "Alloy2B" which is integrated in [ProB Tcl/Tk](https://www3.hhu.de/stups/prob/index.php/Main_Page/).

This repository does not contain a standalone software but the parser to transform an Alloy model to a Prolog term which can then be processed by ProB.
We use the Alloy parser which can be extracted from the [jar-file](http://alloytools.org/download.html).
The actual translation can not be separated from the ProB core due to typechecking and several other dependencies.

[Here](https://www3.hhu.de/stups/prob/index.php/Alloy) you can find some examples how to load Alloy models in ProB.
