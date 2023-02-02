# A Translation from Alloy to B

We introduce a translation of the formal specification language Alloy to classical B.
The translation allows us to load Alloy models into [ProB Tcl/Tk](https://prob.hhu.de/w/) in order to find solutions to the model's constraints.
Our translation closely follows the Alloy grammar, each construct is translated into a semantically equivalent component of the B language.

This repository does not contain a standalone software but the parser to translate an Alloy model to a Prolog term which can then be processed by ProB.
We use the Alloy parser which can be extracted from the [jar-file](http://alloytools.org/download.html).
The actual translation is implemented in Prolog and located in the core of ProB.
Further information on the translation can be found in our [paper](https://stups.hhu-hosting.de/downloads/pdf/Alloy2B.pdf).

[Here](https://prob.hhu.de/w/index.php/Alloy) you can find some examples how to load Alloy models in ProB Tcl/Tk.


# Note
Please send us any Alloy model that cannot be loaded by ProB.

# Developer Note

Gradle: ```compile 'de.hhu.stups:alloy2b:1.0-SNAPSHOT'```

Load an Alloy model from file and translate it to a Prolog term:

```Alloy2BParser parser = new Alloy2BParser();```

```String prologTerm = parser.parseFromFile(alloyFilePath).getPrologTerm();```

The ProB2 Prolog interface provides the predicate ```load_alloy_spec_from_term/2``` to translate the Prolog term to B and load the model into ProB.

#### The Prolog Term

The root of the prolog term is alloy/2:
 
      alloy(RootModule, ListOfModels).

 Each module model is translated to an **alloy_model/5** prolog term.
 
      alloy_model(ModuleName,facts(ListOfAlloyFact),assertions(ListOfAssertion),commands(ListOfCommand),
                  functions(ListOfFunction),signatures(ListOfSignature),ordered_signatures(ListOfAtoms),
                  Options)

 **fact/2**:
 
      fact(Expr,Pos)

 **field/3**:
 
      field(Name,Expr,Options,Pos)
      Options are only 'disj' by now

 **check/8**, **run/8** (functor is either check or run):
 
      functor(FormulaExpr,global_scope(GlobalScope),exact_scopes(ListOfSigAndScope),
              upper_bound_scopes(UpperBoundScopes),bitwidth(BitWidth),maxseq(MaxSeqSize),
              index(CommandIndex),Pos)

 **function/5**, **predicate/5** (functor is either function or predicate):
 
      functor(Name,Params,Decls,Body,Pos)

 **signature/5**:
 
      signature(Name,ListOfFieldDecl,ListOfFact,Options,Pos)
      Options is a subset of [abstract,enum,meta,lone,one,private,some,subset,subsig,top_level]

 **pos/2**:
 
      tuple of x and y position in the Alloy file

 Binary and unary operators are self-explanatory, for instance, a join is represented as the term join/4
 with left and right expression as well as type and position information like
 join(Lhs,Rhs,type/1,pos/2).
