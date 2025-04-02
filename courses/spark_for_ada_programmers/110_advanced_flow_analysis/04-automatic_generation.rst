======================
Automatic Generation
======================

------------------------
From Data Dependencies
------------------------

* Data dependencies may be specified or generated

|

* If flow dependencies are not specified, they are generated

  - All outputs depend on all inputs
  - All globals of mode :ada:`Proof_In` have no effect on outputs

|

* This is a correct over-approximation of actual flow dependencies

  - This might be too imprecise for analysis of callers
  - In that case, add explicit flow dependencies

------------------------
From Flow Dependencies
------------------------

* If only flow dependencies are specified

|

* Data dependencies are generated

  - All variables only on the left-hand side are outputs
  - All variables only on the right-hand side are inputs
  - All other variables are both inputs and outputs

|

* This is the exact data dependencies consistent with flow dependencies

  - Except some globals of mode :ada:`Proof_In` may be classified as inputs

