
***********
Contracts
***********
.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

----------------------
Low-Level Assertions
----------------------

* Language-defined package with procedures

   .. code:: Ada

      package Ada.Assertions is
         Assertion_Error : exception;
         procedure Assert (Check : in Boolean);
         procedure Assert (Check : in Boolean;
                           Message : in String);
      end Ada.Assertions;
 
* Language-defined pragma

   .. code:: Ada

      procedure Push (This : in out Stack;  Value : Content) is
      begin
         -- Same semantics but also can be enabled/disabled 
         pragma Assert (not Full (This));
         ... code that only works if the stack is not full ...
      end Push;
 
* Both raise `Assertion_Error` if expression is False

.. container:: speakernote

   For the pragma, the phrase "any Boolean type" means the language-defined type or any type derived from it.
   The package defines the exception raised by the pragma, and offers procedural forms for assertions.
   The procedures in Ada.Assertions are not controlled by the pragma Assertion Policy (described later).  They are procedures like any other.

-------------------------------
High-Level Assertion Examples
-------------------------------

* Pre- and postconditions specify obligations on subprogram caller and implementer

   .. code:: Ada

      procedure Push (This : in out Stack;  Value : Content) with
         Pre  => not Full (This),
         Post => not Empty (This) and Top (This) = Value;
      ...
      function Top (This : Stack) return Content;
      function Full (This : Stack) return Boolean;
 
* Type invariants ensure properties of abstract data type objects over their lifetimes

   .. code:: Ada

      type Bank_Account is private
        with Type_Invariant => Consistent_Balance(Bank_Account);
      function Consistent_Balance (This : Bank_Account)
                                   return Boolean;

---------------------------------------
Mixing Low- and High-Level Assertions
---------------------------------------

* Useful when trying to prove a difficult high-level form, i.e., subprogram contracts

   - Difficult for the provers to handle

* Gives additional, simpler, incremental information to the provers ...

   - ... and human readers too

.. code:: Ada

   procedure P (...) with 
     Pre  => ...,
     Post => ...;
   procedure P (...) is
   begin
      ...
      pragma Assert (this_is_true_here);
      ...
   end P;
 
===========
Contracts
===========

---------------------
What is a Contract?
---------------------

* Formal agreement between the implementer and the user of a program unit (package or subprogram)
* Assigns responsibilities
* A way to organize your code
* Not a new idea (Floyd, Hoare, Dijkstra, Meyer)

--------------------
Why Use Contracts?
--------------------

* Core idea of contract-based programming:
* Contract between a subprogram and its caller

   - The caller must meet the preconditions specified by the subprogram.
   - In turn, the subprogram must respect the postconditions it has specified.

* New Ada 2012 features support the contract-based programming pattern.

.. container:: speakernote

   Ada 2012 allows arbitrary Boolean expressions to specify behavior contracts on subprograms and types
   A note on terminology:
   the term 'contracts' is preferred to 'annotations' (but not all annotations are contracts)
   the term 'annotations' is preferred to 'aspects' or 'pragmas'
   all annotations may be expressed as pragmas, but the aspect form is preferred if available

-----------------------
Contract Participants
-----------------------

* Contract between a subprogram and its caller

   - The caller must meet the preconditions specified by the subprogram.
   - In turn, the subprogram must respect the postconditions it has specified.

.. code:: Ada

   procedure Increment (X : in out Integer)
      with Pre  => X < Integer'Last,
           Post => X = X'Old + 1;
 
----------------------
Contracts as Aspects
----------------------

* Contracts are written in new aspect notation in Ada 2012:

   .. code:: Ada

      aspect_specification ::=
         with aspect_mark [=> aspect_definition]
              {, aspect_mark [=> aspect_definition] }
 
* An aspect adds information to entities

   .. code:: Ada

      procedure Increment (X : in out Integer)
         with Pre  => X < Integer'Last,
              Post => X = X'Old + 1;
 
---------------
Preconditions
---------------

* Boolean expression 

* Checked before the call to a subprogram
* Constraint to the caller
* Defining expectations under which a subprogram can be called in addition to parameter type constraints
* Relates to the subprogram inputs
* Can be composed of

   - Any visible name in the visible scope of the subprogram (even if defined afterwards)
   - Any parameter of the subprogram

* Part of the specification

-------------------------------------
No Secret Precondition Requirements
-------------------------------------

.. container:: columns

 .. container:: column
  
    * Should only require what the client can ensure

       - By only referencing entities also available to clients

    * Language rules enforce this precept

 .. container:: column
  
    .. code:: Ada
    
       package P is
          type Bar is private;
          ...
          function Foo (This : Bar)
             return Baz
             with Pre => Hidden;
       private
          function Hidden
             return Boolean;
          ...
       end P;
     
.. container:: speakernote

   Illegal because function Hidden is declared in the private part but the reference is in the visible part.
   Clients could never call Hidden (in order to check that they meet the specified precondition content).

----------------------------------
Controlling the Exception Raised
----------------------------------

* Failing pre/postconditions raise `Assertion_Error`
* Our abstractions may define dedicated exceptions

   - Assertion Error

      .. code:: Ada

         type Stack (Capacity : Positive) is tagged private;
         procedure Push (This : in out Stack;  Value : Content) with
           Pre  => not Full (This),
           Post => ...
         function Full (This : Stack) return Boolean;
 
   - Overflow 

      .. code:: Ada

         procedure Push (This : in out Stack;  Value : Content) is
         begin
           if Full (This) then
             raise Overflow;
           end if;
           ...
         end Push;
 
* How to get them raised in preconditions?

   - Not needed for postconditions (failures are supplier bugs)

--------------------------------------
"Raise Expressions" In Preconditions
--------------------------------------

.. code:: Ada

   package Bounded_Stacks is
     type Stack (Capacity : Positive) is tagged private;
     Overflow, Underflow : exception;
     procedure Push (This  : in out Stack;
                     Value : in     Content) with
       Pre  => not Full (This) or else raise Overflow,
       Post => not Empty (This) and Top (This) = Value;
     procedure Pop (This  : in out Stack;
                    Value :    out Content) with
       Pre  => not Empty (This) or else raise Underflow,
       Post => not Full (This);
     function Empty (This : Stack) return Boolean;
     function Full (This : Stack) return Boolean;
   ...
   private
   ...
   end Bounded_Stacks;

----------------
Postconditions
----------------

* Boolean expression 

* Checked after the call to a subprogram
* Constraint to the implementer
* Relates to the subprogram outputs
* Can be composed of

   - Any visible name in the visible scope of the subprogram (even if defined afterwards)
   - Any parameter of the subprogram
   - Values of any of the above before the call to the subprogram
   - Result of a subprogram

* Part of the specification

---------------------------------------
Postconditions Are Good Documentation
---------------------------------------

.. code:: Ada

   procedure Reset
      (Unit   : in out DMA_Controller;
       Stream : DMA_Stream_Selector)
      with Post =>
        not Enabled (Unit, Stream)                   and
        Operating_Mode (Unit, Stream) = Normal_Mode  and
        Current_Counter (Unit, Stream) = 0           and
        Selected_Channel (Unit, Stream) = Channel_0  and
        not Double_Buffered (Unit, Stream)           and
        not Circular_Mode (Unit, Stream)             and
        Memory_Data_Width (Unit, Stream) = Bytes     and
        Peripheral_Data_Width (Unit, Stream) = Bytes and
        Priority (Unit, Stream) = Priority_Low       and
        (for all Flag in DMA_Status_Flag =>
           not Status (Unit, Stream, Flag));

--------------------------
Example - An Observation
--------------------------

* What difference do types make?

.. code:: Ada

   procedure Sqrt (Input : Integer; Res: out Natural)
     with 
          Pre  => Input >= 0,
          Post => (Res * Res) <= Input and
                  (Res + 1) * (Res + 1) > Input;
 
--------------------------------------
Types and Contracts - An Observation
--------------------------------------

* With the help of types... 

   .. code:: Ada

      procedure Sqrt (Input : Integer; Res: out Natural)
        with 
             Pre  => Input >= 0,
             Post => (Res * Res) <= Input and
                     (Res + 1) * (Res + 1) > Input;
 
* ... less to write!

   .. code:: Ada

      procedure Sqrt (Input : Natural; Res: out Natural)
        with 
             Post => (Res * Res) <= Input and
                     (Res + 1) * (Res + 1) > Input;
 
.. container:: speakernote

   The main criticism (still usually minor) towards design by contract is that it burdens the user by requiring to write more (contracts/annotations).
   Good news: with Ada you are better off (than anywhere else)!

------------------------
Observation: Good Fit!
------------------------

* From 83 to 2005, Ada already offers a wide range of contracts

   - Type Ranges

   - Accessibility 

   - Parameter Modes

   - Generic Parameters 
   - Interfaces 

   - Privacy
   - Not Null Access...

-------------------------------------
Where are Type Constraints Checked?
-------------------------------------

* On an assignment / explicit initialization

   .. code:: Ada

      X : Integer := 2;
 
* On a conversion / qualification

   .. code:: Ada

      X : Integer := Integer (1 + Natural'(2));
 
* On a parameter passing

   .. code:: Ada

      procedure P (N : in out Natural);
 
* Intermediate expressions are computed using the base type, with no range check

   .. code:: Ada

      X : Natural := 4;
      Y : Natural := 1 + X + 1;

.. container:: speakernote

   For intermediate operations, such as "+" of "Integer", parameters and result of the op are of Integer'Base.
   There are no range checks on these operations (just overflow checks).
   There will be a range check on the result to be assigned before assignment though.

---------------------------------------
More Facilities for Writing Contracts
---------------------------------------

* Pre and Post
* Assert

* Contract Cases 
* Global 

* Expression Functions

* Expressions 

   - `if` expressions

   - `case` expressions 

   - quantified expressions

.. container:: speakernote

   Some of these we'll talk about now, some postponed to later during the day.
   Most new for Ada 2012, some are revamped SPARK feature.

----------------
Contract Cases
----------------

* New way of expressing postconditions

   .. code:: Ada

      procedure Bounded_Add (X, Y : in Integer; Z : out Integer)
        with Contract_Cases =>
                  (X + Y in Integer      => Z = X + Y,
                   X + Y < Integer'First => Z = Integer'First,
                   X + Y > Integer'Last  => Z = Integer'Last);
 
* Each case has a condition and a consequence
* Conditions are checked for:

   - Completeness 

   - Non-overlap

* `others` may be used

=============
Global Data
=============

------------------
Global Variables
------------------

* Impact code organization 

* Are part of the signature (from the verification perspective)
* May seem innocent at first, but often cause unexpected effects as programs grow.

   - (Not saying that you should not have them, or that they are evil.)

----------------------------
What Is a Global Variable?
----------------------------

.. container:: columns

 .. container:: column
  
    * Any variable that is an input or output of a subprogram (that is not a parameter) is a global

 .. container:: column
  
    .. code:: Ada
    
       procedure Read_Global
          (Value : out Integer)
       is
       begin
          Value := Global; 
       end Read_ Global;
       procedure Global_Above_Zero 
          (Value : out Boolean)
       is
       begin
          Value := (X > 0);
       end Global _Above_Zero;
     
-----------------
Global Contract
-----------------

.. container:: columns

 .. container:: column
  
    * Announces use of global variables
    * Optional mode can be `Input` (default), `Output`, `In_Out` or `Proof_In`
    * `Proof_In` specifies globals that are only referenced within proof expressions within the subprogram

 .. container:: column
  
    .. code:: Ada
    
       procedure P
          with Global =>
          (Input    => (A, B, C),
           Output   => (L, M, N),
           In_Out   => (X, Y, Z),
           Proof_In => (I, J, K));
     
-----------------
Global Contract
-----------------

* Optional 

* So existing code can be analyzed without adding globals

   - Tools will then compute them by default

* But if present, global contract must be complete and correct
* Can be omitted if there are no globals
* `null` gives a positive indication that there are no globals
* The option :command:`--no-global-generation` prohibits global generation, instead assumes `null`

   - **NOTE** Omitted global declaration means global `null` with this option 

.. container:: speakernote

   optional (for both flow analysis and proof) -  haven't explained these yet.

--------------------------------
Functions Without Side-Effects
--------------------------------

* Recall: An expression is side-effect free if its evaluation does not update any object
* Objects updated by subprogram call are any parameters of mode `out` (or `in out`) and any globals of mode `out` (or `in out`)
* So function is side-effect free if all parameters and globals (if any) are of mode `in` only

.. code:: Ada
    
   function A (X : in Integer) return Integer;
   function B (X : in Integer) return Integer
      with Global => null;
   function C (X : in out Integer) return Integer;
   function D (X : in Integer) return Integer
      with Global => (Input => V);
   function H (X : in Integer) return Integer
      with Global => (Output => V);
     
.. container:: speakernote

   A - need to see implementation
   B - OK
   C - no "in out" parameter for function
   D - OK
   E - no global output for function

-----------------------------
Functions With Side-Effects
-----------------------------

.. code:: Ada
    
   function Sqrt (X : in Integer) return Integer;
       
   function Sqrt (X : in Integer) return Integer is
      Result : Integer;
   begin
      Call_Count := Call_Count + 1;
      -- code to calculate Sqrt
      return Result;
   end Sqrt;
     
* What is the correct global contract here?

--------------------------------------
Aliasing Revisited - Input of Global
--------------------------------------

* This declaration is OK

   .. code:: Ada

      procedure Update_Y (X : in Some_Type)
         with Global => (Output => Y);
 
* Aliasing may be detected at the point where a call is made

   .. code:: Ada

      Y, Z : Some_Type;
      ...
      Update_Y (Z); -- OK
      Update_Y (Y); -- Illegal
 
.. container:: speakernote

   Global and Parameter Overlap
   Recall aliasing is detected for overlapping parameters.

---------------------------------------
Aliasing Revisited - Output of Global
---------------------------------------

* As previous example except this time the formal parameter is the export

   .. code:: Ada

      procedure Update_X (X : out Some_Type)
         with Global => (Input => Y);
 
* The declaration above is OK
* Aliasing may be detected at the point where a call is made

   .. code:: Ada

      Y, Z : Some_Type;
      ...
      Update_X (Z); -- OK
      Update_X (Y); -- Illegal
 
-------------------------
Contracts - Test Them! 
-------------------------

* Dynamic Semantics - contracts can be:

   - Compiled 

   - Checked at run time
   - Thought of as "pre-assert" and "post-assert" at the beginning and end of the subprogram body

-------------------------
Contracts - Prove Them! 
-------------------------

* Static Semantics - contracts can be:

   - Interpreted in logic 

   - Formal pre- and post- assertions of a Hoare triplet `{P}S{Q}`, the latter often called  proof obligation, or verification condition
   - Checked exhaustively for all possible inputs by a theorem prover

------------------------
Semantics of Contracts
------------------------

* Recall: contracts themselves may be executed
* Generally, contracts have the same semantics as  Ada

   .. code:: Ada

      procedure P (X, Y, M : in Natural; Z : out Natural)
         with Post => (if X / Y <= M then Z = F_1 (X, Y)) and
                      (if X / Y >  M then Z = F_2 (X, Y));
 
   - Warning: divide by zero might fail

* Observation: Ada semantics of contracts contribute to validation of contracts!

.. container:: speakernote

   Contract Validation
   What could you do to make sure the contract is well-formed?

------------------------------------
Semantics of Contracts (continued)
------------------------------------

.. code:: Ada

   procedure P (X, Y, M : in Natural; Z : out Natural)
      with Post => (if Y /= 0 then
                      (if X / Y <= M then Z = F_1 (X, Y)) and
                      (if X / Y >  M then Z = F_2 (X, Y)));
   procedure P (X, Y, M : in Natural; Z : out Natural)
      with Post => Y /= 0 and then
                      ((if X / Y <= M then Z = F_1 (X, Y)) and
                       (if X / Y >  M then Z = F_2 (X, Y)));
   procedure P (X, Y, M : in Natural; Z : out Natural)
      with Pre  => Y /= 0,
           Post => (if X / Y <= M then Z = F_1 (X, Y)) and
                   (if X / Y >  M then Z = F_2 (X, Y));
   procedure P (X, M : in Natural;
                Y : in Positive;
                Z : out Natural)
      with Post => (if X / Y <= M then Z = F_1 (X, Y)) and
                   (if X / Y >  M then Z = F_2 (X, Y));

.. container:: speakernote

   Strengths, weaknesses of these contracts?
   Which ones will prove?
   Which one will give more work for the caller?

=======================
Language Capabilities
=======================

-----------------------------
New Expressions in Ada 2012
-----------------------------

.. container:: columns

 .. container:: column
  
    * Ada 2012 includes *if expressions*, *case expressions* and *quantified expressions*
    * Quantified expressions (`for all`, `for some`) are particularly useful when writing contracts involving arrays or containers

 .. container:: column
  
       .. code:: Ada
    
          A := (if X then 2 else 3);
          
          B := (case Y
                when E1     => V1,
                when E2     => V2,
                when others => V3);
          
          procedure Set_Array
             (A: out Array_Type)
             with Post =>
             (for all M in A'Range =
                 A(M) = M);
     
----------------------
Expression Functions
----------------------

* Simple functions can be written as expression functions

   .. code:: Ada

      function Is_Full return Boolean is
         (Stack_Pointer = Stack_Size);
      function Value_Found_In_Range (A       : Arr;
                                     Val     : Element;
                                     Low, Up : Index)
                                     return Boolean
         is (for some J in Low .. Up => A(J) = Val);
 
* There is no separate body for such functions
* For proof, the expression forms the postcondition
* Expression functions allow you to write functions in a package specification which can then be used in contracts

------------------------------
More on Expression Functions
------------------------------

* Note that the expression does not have to be boolean:

   .. code:: Ada

      function Add_One (X : in Integer)
            return Integer is (X + 1)
         with Pre => (X < Integer'Last);
 
* Aspect comes after the parenthesized expression

------
'Old
------

* Denotes the value of a parameter or a name (variable, function) before the call to the subprogram
* Is only available in the expression of a postcondition
* Forbidden for limited types
* At implementation level performs a copy of the value, to be used for the check afterwards
* **Note:** making copies of objects may impact performance

---------------
'Old Examples
---------------

.. code:: Ada

   procedure Increment (X : in out Integer)
     with Post => X = X'Old + 1;
   
   procedure Call_Not_Modify_Global
     with Post => Some_Global = Some_Global'Old;
   
   function F (V : SomeRecordT) return Integer
     with Global => Some_Global;
   
   procedure P (V : in out T)
     with Post => 
        V'Old.A   /= V.A and then
        V.B'Old   /= V.B and then
        F (V'Old) /= F (V) and then
        F (V)'Old /= F (V);
 
.. container:: speakernote

   A number of issues with last example:

   1. Potentially unevaluated prefixes!
   2. Efficiency: 'Old will create a copy so consider efficiency. First conjunct: the entire record V will be copied. This can be inefficient compared to approach in second conjunct, where only one component is copied.
   3. What happens when F has global input?

   Also, conditional evaluation is not allowed.

----------
 'Update
----------

* Convenient for expressing in contracts how composite objects have been updated

   .. code:: Ada

      procedure P (R : in out Rec)
        with Post => R = R'Old'Update (X => 1, Z => 5);
      Some_Array'Update (1 .. 10 => True, 5 => False)
      Some_Array'Update (Param_1'Range => True,
                         Param_2'Range => False)
 
* Keeps things simple - for `'Update` of arrays:

   - In contracts: avoids writing quantifiers
   - In code: avoids writing loops

* Can have overlapping and dynamic choice ranges

   - Looks similar to aggregates, but isn't

---------
'Result
---------

.. code:: Ada

   package Find is
      type A is array (1..10) of Integer;
      function Find (T : A; R : Integer) return Integer
         with Post => Find'Result >= 0 and then
                      (if Find'Result /= 0 then
                          T (Find'Result) = R);
   end Find;
 
* `'Result` allows you to use the function result in your contracts.

--------------
Set Notation
--------------

* Another Ada 2012 feature useful for specifications
* Set notation can be used in membership tests
* Without set notation:

   .. code:: Ada

      if not (C = 'A' or else C = 'B' or else C = 'C') then
         ...
      else
         ...
      end if;
 
* With set notation:

   .. code:: Ada

      if C not in 'A' | 'B' | 'C' then
         ...
      else
         ...
      end if;
 
---------------------------
More Subprogram Contracts
---------------------------

* We have seen:

   - Global, Pre, and Post

* Now...

   - Assert
   - Assume

* Later...

   - Depends
   - Loop Invariant, Loop Variant

--------
Assert
--------

* It may be executed or proved or both
* Verification condition is generated for the check, but there is no 'cutpoint' in the proof

.. code:: Ada

   pragma Assert (I in Small_Range);
 
--------
Assume
--------

* No verification condition is generated to prove that the boolean expression is true
* But it is carried forward as though it had been proved true
* Will be checked at run time if assertion checks are on

   .. code:: Ada

      pragma Assume (Ticks < Time_Type'Last);
 
* Soundness alert - use with great care!

========
Lab
========

.. include:: labs/060_contracts.lab.rst

=========
Summary
=========

----------------------------
What To Do With Contracts?
----------------------------

* Improve documentation / readability

   - Specification contains formally expressed properties on the code

* Improve testing

   - Constraints on subprograms and code can lead to dynamic checks enabled during testing

* Improve static analysis

   - The compiler checks the consistency of the properties
   - Static analysis tools (CodePeer) use these properties as part of their analysis

* Perform formal proof

   - Formal proof technologies can prove formally certain properties of the code
