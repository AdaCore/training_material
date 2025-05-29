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

  - Items that only get written to are considered *outputs*
    - LHS of assignment, :ada:`out` parameter of subprogram call

  - Items that only get read are considered *inputs*
    - Not on LHS of assignment, only :ada:`in` parameter

  - All other variables are both inputs and outputs

|

* This is the exact data dependencies consistent with flow dependencies

  - Except some globals of mode :ada:`Proof_In` may be classified as inputs

