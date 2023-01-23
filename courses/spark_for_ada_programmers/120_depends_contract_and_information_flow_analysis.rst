************************************************
Depends Contract and Information Flow Analysis
************************************************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

---------------------------
Information Flow Analysis
---------------------------

* Data flow analysis - recap
* Information flow analysis - deeper analysis
* **Depends Contracts** - contracts for information flow
* **Global Contracts** state the global variables accessed or modified by a subprogram

===============
Flow Analysis
===============

------------------------
Flow Analysis - Recap
------------------------

* Static analysis performed by :toolname:`GNATprove`
* Models the variables used by a subprogram

   - Variables (global and local)
   - Formal parameters

* Models how information flows through the statements in the subprogram

   - From initial values of variables
   - To final values of variables

* Performs checks and detects errors

   - Use of uninitialized variables, ineffective statements, unused variables... what else?

------------------------------------------
Flow Analysis - Incorrect Parameter Mode
------------------------------------------

* Detection of incorrect mode of parameters (`in`, `out`, or `in out`)

.. code:: Ada

   procedure Swap (X, Y : in out T) is
     Tmp : T := X;
   begin
     Y := X;    -- The initial value of Y is not used
     X := Tmp;  -- Y is computed to be out
   end Swap;

.. list-table::
   :header-rows: 1
   :width: 90%

  * - Initial value read

    - Some path updated
    - Every path updated
    - Parameter mode

  * - |checkmark|

    -
    -

    - In

  * - |checkmark|

    - |checkmark|
    - |checkmark|
    - In out

  * -

    - |checkmark|

    -

    - In out

  * -

    -

    - |checkmark|
    - out

.. container:: speakernote

   Parameter modes influence the behavior of the compiler and are a key point for documenting the usage of a subprogram.
   Flow analysis will check that specified parameter modes always correspond to their usage in the subprogram's body.
   More precisely, it will check that an "in" parameter is never updated, either directly or through a subprogram call.
   It will also check that the initial value of an "out" parameter will never be read in the subprogram, as it may not be copied on subprogram entry.
   Finally, flow analysis will also warn when an in out parameter is not updated or when its initial value is not used in the subprogram, as it may be the sign of an error, like in the procedure Swap.

----------------------------------
Flow Analysis - Global Contracts
----------------------------------

* **Global Contracts** state the global variables accessed or modified by a subprogram

   - Variables are global to a subprogram if they are defined outside of its scope (at library level or in enclosing units for nested subprograms)

* They are checked by flow analysis when present

   - Flow analysis makes sure they are complete (no global variable missing) and correct

.. code:: Ada

   X : Natural := 0;
   function Get_Value_Of_X return Natural;
   -- Get_Value_Of_X reads value of global variable X

.. container:: speakernote

   Until now, we have seen verifications which do not require any additional annotations from the user.
   Flow analysis will also check user-written flow annotations when supplied.
   In SPARK, it is possible to specify the global and scoped variables accessed or modified by a subprogram.
   This is done using an Ada 2012 like contract named Global.
   When a Global contract is supplied by the user for a subprogram, flow analysis will check that it is correct and complete, that is, no other variable than those stated in the contract are accessed or modified, either directly or through a subprogram call.
   For example, we may want to specify that the function GetValueOfX reads the value of the global variable X and does not access any other global variable.

----------------------------------
Flow Analysis - Global Contracts
----------------------------------

* `Global` contracts are part of the specifications

   - Like parameters, they have a mode. It can be `Input`, `Output`, `In_Out`, or `Proof_In` (for global variables only referenced in assertions)
   - Default mode is `Input`
   - `null` can be used to specify that no global variable is referenced

.. code:: Ada

   procedure Set_X_To_Y_Plus_Z with
     Global => (Input  => (Y, Z), -- reads values of Y and Z
                Output => X);     -- modifies value of X
   procedure Set_X_To_X_Plus_Y with
     Global => (Input  => Y,  -- reads value of Y
                In_Out => X); -- modifies value of X
                              -- also reads its initial value
   function Get_Value_Of_X return Natural with
     Global => X;  -- reads the value of the global variable X
   procedure Incr_Parameter_X (X : in out Natural) with
     Global => null; -- does not reference any global variable

.. container:: speakernote

   Global contracts are part of the specification.
   Indeed, they provide useful information to users of a subprogram.
   The value specified for the Global aspect is an aggregate-like list of global variables' names, grouped together depending on their mode.
   For example, the procedure SetXToYPlusZ reads both Y and Z, listed as Input, and updates X, listed as Output.
   As SetXToXPlusY both updates X and reads its initial value, X's mode is InOut.
   Like for parameters, if no mode is specified, then the default is Input.
   That is the case in the declaration of GetValueOfX.
   Finally, if a subprogram, like IncrParameterX, does not reference any global variable, the value of the global contract should be set to 'null'.

-------------------------
Different Flow Analyses
-------------------------

* The errors seen so far can be detected regardless of whether flow contracts are present or not

   - Because they can be detected from the code alone
   - These errors are detected by data flow analysis

* The errors in the following sections require flow contracts

   - Because they relate to discrepancies between the contract and the code
   - These errors are detected by information flow analysis

===================
Depends Contracts
===================

------------------
Depends Contract
------------------

* Contract for information flow analysis
* Announces dependencies between outputs and inputs

   .. code:: Ada

      procedure Sum (A, B : in Integer; Result : out Integer)
        with Depends => (Result => (A, B));

   - Outputs appear on the left-hand side of the arrow
   - Inputs appear on the right

* Can be read as **Output depends on Input** (in this case *Result depends on A and B*)

-------------------
Depends Contracts
-------------------

* `Depends` contracts are part of the specifications
* `Depends` contracts:

   - Takes into account both parameters and global variables
   - Useful in particular for checking security properties
   - Optional

.. code:: Ada

   procedure Swap (X, Y : in out T);
   -- The value of X (or Y) after the call depends only
   -- on the value of Y (or X) before the call

   X : Natural;
   procedure Set_X_To_Zero;
   -- The value of X after the call depends on no input

.. container:: speakernote

   A user may also supply a Depends contract for a subprogram to specify dependencies between its outputs and its inputs.
   Here, not only global variables are considered but also parameters and function result.
   When a Depends contract is supplied for a subprogram, flow analysis checks that it is correct and complete, that is, that each subprogram output is related to all of its inputs.
   For example, a user may want to check that, on return of Swap, each parameter only depends on the initial value of the other parameter or that the value of X on return of SetXToZero does not depend on any global variable.

------------------
Depends Contract
------------------

* **+** indicates a self-dependency

.. code:: Ada

   procedure Update_Array (A : in out Array_Type;
                           I : in     Index_Type;
                           X : in     Elem_Type)
     with Depends => (A => +(I, X));
     -- Equivalent to writing Depends => (A => (A, I, X))

------------------
Depends Contract
------------------

* `null` can be used to specify that an output depends on no inputs

   .. code:: Ada

      procedure Clear_Stack (S : out Stack)
        with Depends => (S => null);

   - S does not depend on any input

* `null` can be used to specify that a list of (one or more) inputs do not affect any outputs

   .. code:: Ada

      procedure Do_Nothing (X, Y, Z : in T)
        with Depends => (null => (X, Y, Z));

   - No output depends on X, Y, or Z

* `null` output can only appear once and must be last

---------------------------
Information Flow Analysis
---------------------------

* Information-flow analysis checks body against `Depends` annotation

   - When a `Depends` contract is specified for a subprogram, it should be complete (relate every output to all its inputs) and correct.

.. code:: Ada

   procedure Q (X, Y : in out Integer)
      with Depends => (X => Y, Y => X)
   is
   begin
      X := X + 1;
      Y := Y + 1;
   end Q;

.. code:: console

   medium: missing dependency "X => X"
   medium: incorrect dependency "X => Y"
   medium: missing dependency "Y => Y"
   medium: incorrect dependency "Y => X"

--------------------------
Arrays and Flow Analysis
--------------------------

* Recall: Flow analysis treats array objects as single, entire objects
* Updating and reading components of records is analyzed in terms of those components

   - Not treated as operations on records in their entirety

--------------------------
Arrays and Flow Analysis
--------------------------

* Recall: If possible, use an aggregate to initialize the array

   .. code:: Ada

      procedure Init_Array (A : out Bool_Array_Type) is
      begin
         A := (1 .. 2 => True, others => False);
      end Init_Array;

* Flow analysis knows that the array must be fully defined by the aggregate so no errors are reported

--------------------------------
Flow Analysis - Path Sensitive
--------------------------------

* Flow analysis, and in particular detection of uninitialized variables, is done modularly on a per subprogram basis

   - Global and parameter inputs should be initialized prior to any subprogram call.
   - Global and parameter outputs should be initialized prior to subprogram return.

.. code:: Ada

  procedure Set_X_To_Y_Plus_Z
    (Y, Z     :     Natural;
     X        : out Natural;
     Overflow : out Boolean) is
  begin
    if Natural'Last - Z < Y then
      Overflow := True;
      -- X not initialized on every path
    else
      Overflow := False;
      X := Y + Z;
    end if;
  end Set_X_To_Y_Plus_Z;

.. container:: speakernote

   Flow analysis is a sound analysis, which means that, if it does not output any message on some analyzed SPARK code, then none of the supported errors may occur in this code.
   On the other hand, they are cases where flow analysis will issue a message when there in fact are no errors.
   The first, and maybe most common, reason for this has to do with modularity.
   To improve efficiency on large projects, verifications are in general done on a per subprogram basis.
   It is in particular the case for detection of uninitialized variables.
   For this detection to be done modularly, flow analysis needs to assume initialization of inputs on subprogram entry and initialization of outputs after subprogram calls.
   Therefore, every time a subprogram is called, flow analysis will check that global and parameter inputs are initialized, and every time a subprogram returns, it will check that global and parameter outputs are initialized.
   This may lead to messages being issued on perfectly correct subprograms like SetXToYPlusZ which only sets its out parameter X when Overflow is false.
   This simply means that, in that case, flow analysis was not able to verify that no uninitialized variable could be read.
   To solve this problem, X can either be set to a dummy value when there is an overflow or the user can verify by her own means that X is never used after a call to SetXToYPlusZ if Overflow is True.

====================
Contract Synthesis
====================

------------------------------------
Flow Analysis Contract Computation
------------------------------------

* When not specified for a subprogram, `Global` and `Depends` contracts are computed (synthesized)

   - Computed `Global` contracts are used to check initialization of variables
   - Computed contracts of callees are used to check the user-written contracts of callers.

* Sometimes, computed contracts are not precise enough

   - Global variable may have mode `In_Out` instead of `Output`
   - `Depends` contracts always assume that all outputs depend on all inputs.

* Use :command:`--no-global-generation` for fully modular behavior

---------------------------------
Nature of Synthesized Contracts
---------------------------------

* Are conservative, guaranteed safe for analysis

   - May specify more dependencies than actually exist, but not fewer

* Not checked in the corresponding body

   - Likely does not match body since more conservative
   - Would generate false positives
   - But safe so analysis is still sound

-------------------------------------
How Flow Contracts Are Synthesized
-------------------------------------

* For each of `Global` and `Depends` contracts, synthesis takes the existence of the other into account, if present

  * The body content is also taken into account

* Synthesize contracts when:

  * `Global` contract without `Depends` contract

    - Synthesize a `Depends` contract indicating each output depends on all inputs

  * `Depends` contract without `Global` contract

    - Synthesize a `Global` contract based on the content of the `Depends` contract

  * No flow contracts, body exists

    - Synthesize a `Global` contract deduced from subprogram body
    - Synthesize a `Depends` contract based on that generated Global

  * No flow contracts, body does not exist

    - Synthesize a `Global` contract indicating no objects referenced
    - Synthesize a `Depends` contract indicating each output depends on all inputs

--------------------------------------
When Only Global Contracts Specified
--------------------------------------

* No `Depends` contract
* Synthesizes a `Depends` contract specifying all outputs depending on all inputs

   - Hence "conservative"

* Allows detecting all possible initialization and contract violation errors

   - In the subprogram

   - In the subprogram's callers

* May lead to false alarms because imprecise

   - I.e., conservative

--------------------------------------
Synthesized Depends Contract Example
--------------------------------------

.. code:: Ada

   package Only_Data_Dependencies with
     SPARK_Mode
   is
      V : Integer;
      procedure Add (X : Integer) with
        Global => (In_Out => V);
        -- synthesized 'Depends' would look like:
        Depends => (V =>+ X)
        -- with all outputs depending on all inputs
      procedure Swap (X : in out Integer) with
        Global => (In_Out => V);
        -- synthesized 'Depends' would look like:
        Depends => ((X, V) => (X, V))
        -- with all outputs depending on all inputs
      procedure Call_Add (X, Y : Integer) with
        Global  => (In_Out => V),
        Depends => (V =>+ (X, Y));
      procedure Call_Swap (X, Y : in out Integer) with
        Global  => (In_Out => V),
        Depends => (X => Y, Y => X, V => V);
   end Only_Data_Dependencies;

.. container:: speakernote

   Depending on the bodies, other flow dependencies with fewer dependencies between inputs and outputs would be compatible with the given data dependencies of Add and Swap.
   GNATprove chooses the contracts with the most dependencies.

---------------------------------------
When Only Depends Contracts Specified
---------------------------------------

* No `Global` contract
* Synthesizes a `Global` contract compatible with content of the `Depends` contract

.. code:: Ada

   package Only_Flow_Dependencies with SPARK_Mode is
      V : Integer;
      procedure Add (X : Integer) with
        Global => (In_Out => V),
        Depends => (V =>+ X);
      procedure Swap (X : in out Integer) with
        Global => (In_Out => V),
        Depends => (V => X, X => V);
      procedure Call_Add (X, Y : Integer) with
        ...
      procedure Call_Swap (X, Y : in out Integer) with
        ...
   end Only_Flow_Dependencies;

--------------------------------------
Synthesis When No Contract Specified
--------------------------------------

* A `Global` contract deduced from subprogram body
* A `Depends` contract based on that generated Global
* Precise because based on actual body

   - Uses path-sensitive flow analysis to track data flows in the subprogram body

* The above is for code in SPARK

   - (When code is not in SPARK will be covered momentarily)

--------------------------------------
Example When Neither Given, In SPARK
--------------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       package Gen_Global with
         SPARK_Mode
       is
         V : Boolean;
         procedure Set_Global with
           Global  => (Output => V),
           Depends => (V => null);
         procedure Do_Nothing;
         procedure Set_Global_Twice;
       end Gen_Global;

 .. container:: column

    .. code:: Ada

      package body Gen_Global with
        SPARK_Mode
      is
        procedure Set_Global is
        begin
          V := True;
        end Set_Global;
        procedure Do_Nothing is null;
        procedure Set_Global_Twice is
        begin
          Set_Global;
          Set_Global;
        end Set_Global_Twice;
      end Gen_Global;

-----------------------------------------
Synthesis When The Body Is Not In SPARK
-----------------------------------------

* "Coarse" data and flow contracts computed, based on body content

* A variable written in the body is considered both an input and an output
* A variable only read is considered an input

* All outputs are considered to depend on all inputs

------------------------------------------
Example When Neither Given, Not In SPARK
------------------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       package Gen_Global with
         SPARK_Mode
       is
          V : Boolean;
          procedure Set_Global with
            Global  => (In_Out => V),
            Depends => (V => V);
          procedure Do_Nothing;
          procedure Set_Global_Twice;
       end Gen_Global;

 .. container:: column

    .. code:: Ada

       package body Gen_Global with
         SPARK_Mode
       is
         procedure Set_Global with
           SPARK_Mode => Off is
         begin
           V := True;
         end Set_Global;
         procedure Do_Nothing is null;
         procedure Set_Global_Twice is
         begin
           Set_Global;
           Set_Global;
         end Set_Global_Twice;
       end Gen_Global;

========
Lab
========

.. include:: labs/120_depends_contract_and_information_flow_analysis.lab.rst

=========
Summary
=========

---------
Summary
---------

* Global contracts

   - Forces programmer to specify global data interaction

* Depends contracts

   - Forces programmer to specify data flow

* Without these contracts, flow analysis can still be performed

   - But it will be based on code as *written* as opposed to *requirements*
