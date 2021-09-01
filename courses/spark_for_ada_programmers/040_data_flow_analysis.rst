********************
Data Flow Analysis
********************

.. |rightarrow| replace:: :math:`\rightarrow`
.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

----------
Overview
----------

* What is flow analysis?
* Which errors does it detect?
* How does flow analysis work?
* Flow analysis of composite objects
* Flow analysis using :toolname:`GNATprove`

=======================
What is Flow Analysis
=======================

------------------------
What is Flow Analysis?
------------------------

* Static analysis performed by :toolname:`GNATprove`
* Models the variables used by a subprogram

   - Global variables
   - Local variables
   - Formal parameters

* Models how information flows through the statements in the subprogram

   - From initial values of variables
   - To final values of variables

* Performs checks and detects errors...

.. container:: speakernote

   Don't forget to cover analysis across packages which can detect use of uninitialized variables at startup
   Don't forget to explain when we deal with entire variables and when we deal with subcomponents

--------------------------------
Detecting Errors (or "Issues")
--------------------------------

* What Classes of Errors Does It Detect?

   - Use of uninitialized variables
   - Ineffective statements

      + Statements which update variables
      + But which have no effect on the final value of any variable

   - Detecting unused variables

      + Not considered an error in SPARK
      + But probably is a coding error
      + Generates warnings

* More classes of errors can be detected if contracts are added, more about that later...

.. container:: speakernote

   "Issue" we use instead of saying bug to the customer :)

--------------------------------
Use of Uninitialized Variables
--------------------------------

   .. code:: Ada

      procedure P (Y : out Integer) is
         X : Integer;
      begin
         X := X + 1;
         Y := X;
      end P;
      procedure Inc (X : in out Integer);
      procedure R (Y : out Integer) is
      begin
         Inc (Y);
      end R;
      procedure Outer is
         Y : Integer;
         procedure Inner is
         begin
            Y := Y + 1; -- This is not the error!
         end Inner;
      begin
         Inner; -- This is the error!
      end Outer;

.. container:: speakernote

   Each example shows a different example of use of an uninitialized variable that can be detected by the tools.
   In the last case it's the call to Inner which is wrong, not the assignment Y := Y + 1.

------------------------
Ineffective Statements
------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       procedure P
         (X, Y : in Integer;
          Z    : out Integer)
       is
          T : Integer;
       begin
          T := X + 1;
          T := T + Y;
          Z := 3;
       end P;

 .. container:: column

    * Have no effect on any output variable

       - Therefore no effect on behavior of code

    * Are different from dead code

       - They are executed
       - May modify some variables, but not outputs

    * Usually indicate a coding error

----------------------------
Detecting Unused Variables
----------------------------

.. code:: Ada

   Tmp : Integer;
   procedure Swap2 (X, Y : in out Integer) is
     Temp : Integer := X;
     -- warning: initialization of "Temp" has no effect
     -- warning: variable "Temp" is not referenced
   begin
     X := Y;
     Y := Tmp;
   end Swap2;

.. container:: speakernote

   Probably a spelling error!

--------------------------
Incorrect Parameter Mode
--------------------------

 .. list-table::
   :header-rows: 1

  * - Initial Value

    - Some Path Updated

    - Every Path Updated

    - Parameter Mode

  * - |checkmark|

    -

    -

    - in

  * - |checkmark|

    - |checkmark|

    - |checkmark|

    - in out

  * -

    - |checkmark|

    -

    - in out

  * -

    -

    - |checkmark|

    - out

* Detection of incorrect mode of parameters (`in`, `out`, or `in out`)

.. code:: Ada

   procedure Swap (X, Y : in out T) is
     Tmp : T := X;
   begin
     Y := X;   -- The initial value of Y is not used
     X := Tmp; -- Y is computed to be out
   end Swap;

.. container:: speakernote

   Parameter modes influence the behavior of the compiler and are a key point for documenting the usage of a subprogram.
   Flow analysis will check that specified parameter modes always correspond to their usage in the subprogram's body.
   More precisely, it will check that an "in" parameter is never updated, either directly or through a subprogram call.
   It will also check that the initial value of an "out" parameter will never be read in the subprogram, as it may not be copied on subprogram entry.
   Finally, flow analysis will also warn when an in out parameter is not updated or when its initial value is not used in the subprogram, as it may be the sign of an error. An example is shown on this slide in the subprogram called Swap.
   Note that, in SPARK, a parameter which is not read but not updated on every path should be declared as in out as its final value may depend on its initial value.

==============================
Why Do We Need Flow Analysis
==============================

---------------------------------
Flow Analysis - Why Do We Care?
---------------------------------

* A common source of errors

   - See CWE (Common Weakness Enumeration)/SANS list of "Most Dangerous Software Errors"

   - http://cwe.mitre.org/top25/

* Ensure there is no dead code

* Improper initialization errors are listed as one of the most dangerous programming errors

   - The SPARK flow analysis identifies all variables that have not been initialized prior to them being read.

   - If the SPARK flow analysis finds no uninitialized variables, then there really are none!

* Identifying ineffective statements
* Well-defined basis for deeper analysis (proof)
* Security properties

   - Advanced flow analysis with contracts, more about this later...
   - Not sound in the presence of some of these errors

.. container:: speakernote

   For the final bullet we could equally add 'safety properties', i.e. don't want to derive high integrity data from low integrity data.

------------
Modularity
------------

* Flow analysis, and in particular detection of uninitialized variables, is done modularly on a per subprogram basis

   - Global and parameter inputs should be initialized prior to any subprogram call.
   - Global and parameter outputs should be initialized prior to subprogram return

.. code:: Ada

   procedure Set_X_To_Y_Plus_Z (Y, Z     :     Natural;
                                X        : out Natural;
                                Overflow : out Boolean) is
   begin
     if Natural'Last - Z < Y then
       Overflow := True;
       -- X should be initialized on every path
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

------------------
Value Dependency
------------------

* Flow analysis is not value dependent

   - It only reasons in terms of control flow

.. code:: Ada

   procedure Absolute_Value (X : Integer; R : out Natural) is
   begin
     if X < 0 then
       R := -X;
     end if;
     if X >= 0 then
       R := X;
     end if;
   end Absolute_Value;

* Flow analysis does not know that `R` is initialized

.. code:: Ada

   procedure Absolute_Value (X : Integer; R : out Natural) is
   begin
     if X < 0 then
       R := -X;
     else
       R := X;
     end if;
   end Absolute_Value;

* Flow analysis knows that `R` is initialized

.. container:: speakernote

   It is also worth noting that flow analysis is not value dependent, in the sense that it never reasons about values of expressions.
   As a consequence, if some path in the subprogram are impossible due to values of expressions, it will still consider them feasible and therefore may emit unnecessary messages concerning them.
   On the left version of AbsoluteValue for example, flow analysis computes that, on a path entering none of the two conditional statements, R is uninitialized.
   As it does not consider values of expressions, it cannot know that such a case can never happen.
   To avoid this problem, it is better to make the control flow explicit, as in the right hand version of AbsoluteValue.

------------------------------------
Flow Analysis of Composite Objects
------------------------------------

* Arrays

   - Flow analysis treats array objects as single, entire objects
   - Changing one element of an array object preserves the value of the other elements

* Records

   - Updating and reading of fields of records is analyzed in terms of those fields

   - Updates and reads not treated as operations on records in their entirety

=========================
Flow Analysis on Arrays
=========================

--------------------------
Arrays and Flow Analysis
--------------------------

* Consider the following types and objects

   .. code:: Ada

      type Index is range 1 .. 2;
      type T is array (Index) of Boolean;
      A    : T;
      I, J : Index;

* (An array with one element is not very interesting!)

----------------------------------
Array Elements and Flow Analysis
----------------------------------

* Let's assume the initial value of `A` is (x, y), and we execute the assignment statement:

   .. code:: Ada

      A (I) := True;

* The final value of `A` might be (True, y) or (x, True) depending on the value of `I`
* Note that at least one element of the initial value is preserved in both cases
* So the final value of `A` must depend on:

   - Initial value of `A` (because one element of `A` is preserved)
   - Value of `I` (because `I` specifies which element of `A` is updated)

--------------------------
Array Element Assignment
--------------------------

* Let's assume the initial value of `A` is (x, y), and we execute the assignment statements:

   .. code:: Ada

      A (I) := True;
      A (J) := True;

* Three possible outcomes:

|
   :(True, y): if I = J = 1
   :(x, True): if I = J = 2
   :(True, True): if I /= J

* Flow analysis doesn't know the values of `I` and `J` so conservatively assumes that `A` depends on `A`, `I` and `J`

------------------
Array Assignment
------------------

* In general, there is no way for the flow analyzer to determine whether a sequence of assignments to an array has updated all the elements of the array or a subset of them
* So initializing an array with a sequence of statements will result in a flow error

   .. code:: Ada

      procedure Init_Array (A : out T) is
      begin
         A (1) := True; -- flow error
         A (2) := True;
      end Init_Array;

* Don't "fix" this by changing the mode of `A` to `in out`

----------------------
Array Initialization
----------------------

* If possible, use an aggregate to initialize the array

   .. code:: Ada

      procedure Init_Array (A : out T) is
      begin
         A := (1 .. 2 => True);
      end Init_Array;

   - (`others` could be used in this particular instance)

* Flow analysis knows that the array must be fully defined by the aggregate, so no errors are reported

=============================
Using :toolname:`GNATprove`
=============================

-------------------------------------------
Flow Analysis Using :toolname:`GNATprove`
-------------------------------------------

.. container:: columns

 .. container:: column

    * From the command line:

       - :command:`gnatprove --mode=flow`

 .. container:: column

    .. image:: ../../images/spark_menu-examine_file.jpeg

========
Lab
========

.. include:: labs/040_data_flow_analysis.lab.rst

==========
Summary
==========

----------
Summary
----------

- Data Flow Analysis helps in achieving Bronze Level verification

   + No reading of uninitialized data
   + No interference bewteen parameters and global data
   + No unintended access to global data
