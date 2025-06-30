===============
Flow Analysis
===============

--------------------------------------
Uncontrolled Data Visibility Problem
--------------------------------------

.. image:: subprograms_accessing_global.png

* Effects of changes are **potentially pervasive** so one must understand
  everything before changing anything

---------------------------
Data Dependency Contracts
---------------------------

* Introduced by aspect :ada:`Global`
* Optional, but must be complete if specified
* Optional mode can be :ada:`Input` (default), :ada:`Output`, :ada:`In_Out`
  or :ada:`Proof_In`

  .. code:: Ada

     procedure Proc
     with
       Global => (Input    => X,
                  Output   => (Y, Z),
                  In_Out   => V,
                  Proof_In => W);

* :ada:`Proof_In` used for inputs **only** referenced in **assertions**
* :ada:`Global => null` used to state that no global variable is read/written
* Functions can have only :ada:`Input` and :ada:`Proof_In` global variables

  - Remember: no side-effects in functions!

----------------------------
Data Initialization Policy
----------------------------

* Subprogram :dfn:`inputs` are input parameters and globals

  - parameters of mode :ada:`in` and :ada:`in out`
  - global variables of mode :ada:`Input` and :ada:`In_Out`

* Subprogram :dfn:`outputs` are output parameters and globals

  - parameters of mode :ada:`out` and :ada:`in out`
  - global variables of mode :ada:`Output` and :ada:`In_Out`

* Inputs should be completely initialized **before** a call
* Outputs should be completely initialized **after** a call
* Stricter policy than in Ada

  - Allows **modular analysis** of initialization
  - Relaxed initialization will be seen in course on Advanced Proof

--------------------------
Stricter Parameter Modes
--------------------------

**Initial Read** - Initial value read

**Partial Write** - Object partially written: either part of the object
written, or object written only on some paths, or both

**Full Write** - Object fully written on all paths

 .. list-table::
   :header-rows: 1

  * - Initial Read

    - Partial Write
    - Full Write
    - Parameter Mode

  * - |checkmark|

    -

    -

    - :ada:`in`

  * - |checkmark|

    - |checkmark|

    -

    - :ada:`in out`

  * - |checkmark|

    -

    - |checkmark|
    - :ada:`in out`

  * -

    - |checkmark|

    -

    - :ada:`in out`

  * -

    -

    - |checkmark|
    - :ada:`out`

* Similar rules for modes of global variables

----------------------------------------------
Violations of the Data Initialization Policy
----------------------------------------------

.. container:: columns

 .. container:: column

    * Parameter only partially written should be of mode :ada:`in out`

    |

    .. code:: ada

       procedure Cond_Init
         (X    : out T;
          -- Incorrect
          Cond : Boolean)
       is
       begin
          if Cond then
             X := ..;
          end if;
       end Cond_Init;

 .. container:: column

    * Global variable only partially written should be of mode :ada:`In_Out`

    .. code:: ada

       X : T;
       procedure Cond_Init
         (Cond : Boolean)
       with
         Global => (Output => X)
         -- Incorrect
       is
       begin
          if Cond then
             X := ..;
          end if;
       end Cond_Init;

-----------------------------------------
Generation of Data Dependency Contracts
-----------------------------------------

* :toolname:`GNATprove` computes a correct approximation

  - Based on the implementation
  - Using either specified or generated contracts for calls
  - More precise generation for SPARK code than for Ada code

|

* Generated contract may be imprecise

  - Output may be computed as both input and output

    + Because it is not known if the initial value is read
    + Because it is not known if the object is fully written on all paths

  - Precision can be recovered by adding a user contract

--------------
Bronze Level
--------------

* Check that each object read has been initialized
* Check that code respects data dependency contracts

  .. code:: Ada

     procedure Swap (X, Y : in out Integer)
     with
       Global => null; -- Wrong

     procedure Swap (X, Y : in out Integer) is
     begin
        Temp := X;
        X := Y;
        Y := Temp;
     end Swap;

* Errors for most serious issues, need fixing for proof
* Warn on unused variables, ineffective statements

---------------
Flow Warnings
---------------

* Ineffective statement = statement without effects

  - Dead code
  - Or statement does not contribute to an output
  - Or effect of statement is hidden from :toolname:`GNATprove`

* Warnings can be suppressed with pragma :ada:`Warnings`

  .. code:: ada

     pragma Warnings (Off, "statement has no effect",
                      Reason => "debug");
     Debug_Print (X);
     pragma Warnings (On, "statement has no effect");

* Optional first pragma argument :ada:`GNATprove` indicates it is specific to
  :toolname:`GNATprove`

