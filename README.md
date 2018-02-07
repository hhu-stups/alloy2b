# A Translation from Alloy to B

[![Build Status](https://travis-ci.org/hhu-stups/alloy2b.svg?branch=master&style=flat-square)](https://travis-ci.org/hhu-stups/alloy2b)

We introduce a translation of the specification language Alloy to classical B.
The translation allows us to load Alloy models into \prob\ in order to find solutions to the model's constraints.
Our translation closely follows the Alloy grammar, each construct is translated into a semantically equivalent component of the B language.
In addition to basic Alloy constructs, our approach supports integers and orderings.
The translation is fully automated by the tool \enquote{Alloy2B}.
