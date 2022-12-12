
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
         procedure Assert (Check   : in Boolean;
                           Message : in String);
      end Ada.Assertions;

* Language-defined pragma

   .. code:: Ada

      procedure Push (This : in out Stack; Value : Content) is
      begin
         -- Same semantics but also can be enabled/disabled
         pragma Assert (not Full (This));
         -- Code that only works if the stack is not full ...
      end Push;

* Both raise `Assertion_Error` if expression is False

.. container:: speakernote

   For the pragma, the phrase "any Boolean type" means the language-defined type or any type derived from it.
   The package defines the exception raised by the pragma, and offers procedural forms for assertions.
   The procedures in Ada.Assertions are not controlled by the pragma Assertion Policy (described later).  They are procedures like any other.

-------------------------------
High-Level Assertion Examples
-------------------------------

* Preconditions and postconditions specify obligations on subprogram caller and implementer

   .. code:: Ada

      procedure Push (This : in out Stack; Value : Content) with
         Pre  => not Full (This),
         Post => not Empty (This) and Top (This) = Value;
      ...
      function Top (This : Stack) return Content;
      function Full (This : Stack) return Boolean;

* Type invariants ensure properties of abstract data type objects over their lifetimes

   .. code:: Ada

      type Bank_Account is record ... end record
        with Type_Invariant => Consistent_Balance(Bank_Account);
      function Consistent_Balance (This : Bank_Account)
                                   return Boolean;

--------------------------------------------
Mixing Low-Level and High-Level Assertions
--------------------------------------------

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

======================
Subprogram Contracts
======================

---------------------
What is a Contract?
---------------------

* Formal agreement between the implementer and the user of a subprogram
* Assigns responsibilities
* A way to organize your code
* Not a new idea (Floyd, Hoare, Dijkstra, Meyer)

--------------------
Why Use Contracts?
--------------------

* Core idea of contract-based programming
* Contract between a subprogram and its caller

   - The caller must meet the preconditions specified by the subprogram.
   - In turn, the subprogram must respect the postconditions it has specified.

* Ada 2012 features support the contract-based programming pattern.

  .. code:: Ada

     procedure Increment (X : in out Integer)
       with Pre  => X < Integer'Last,
            Post => X = X'Old + 1;

.. container:: speakernote

   Ada 2012 allows arbitrary Boolean expressions to specify behavior contracts on subprograms and types
   A note on terminology:
   the term 'contracts' is preferred to 'annotations' (but not all annotations are contracts)
   the term 'annotations' is preferred to 'aspects' or 'pragmas'
   all annotations may be expressed as pragmas, but the aspect form is preferred if available

----------------------
Contracts as Aspects
----------------------

* Contracts are written in the aspect notation of Ada 2012:

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
   - Result of a function

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

---------------------------------------
More Facilities for Writing Contracts
---------------------------------------

* Pre and Post

* Contract Cases

* Expression Functions

* Expressions

   - `if` expressions

   - `case` expressions

   - quantified expressions

   - declare expressions

   - etc.

.. container:: speakernote

   Some of these we'll talk about now, some postponed to later during the day.
   Most new for Ada 2012, some are revamped SPARK feature.

----------------
Contract Cases
----------------

* New way of expressing postconditions

   .. code:: Ada

      procedure Bounded_Add (X, Y : in Integer;
                             Z    : out Integer)
        with Contract_Cases =>
           (X + Y in Integer      => Z = X + Y,
            X + Y < Integer'First => Z = Integer'First,
            X + Y > Integer'Last  => Z = Integer'Last);

* Each case has a guard and a consequence
* Guards are checked for:

   - Completeness

   - Non-overlap

* `others` may be used

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

========================
Specification Features
========================

---------------------------------
New Features For Specifications
---------------------------------

* New Features in Ada 2012 or Ada 2022

* Most features can be used both in code and in assertions

* Main benefit is for specifications

* New attributes: `'Old`, `'Result`

* New expressions: set notation, *if expressions*, *case expressions*,
  *quantified expressions*, *declare expressions*, *delta aggregates*

* New declarations: *expression functions*

-----------------------------------
New Attributes For Specifications
-----------------------------------

* Attribute `'Old` and `Result` can be used only in postconditions (and
  consequences of contract cases)

* Attribute `'Old` refers to the value of an object at subprogram entry

  .. code:: Ada

     procedure Increment (X : in out Integer)
       with Post => X = X'Old + 1;

* Attribute `'Result` refers to the value returned by a function

  .. code:: Ada

     function Increment (X : Integer) return Integer
       with Post => Increment'Result = X + 1;

----------------
Attribute 'Old
----------------

* Dynamic semantics is to make a copy at subprogram entry

  - Forbidden on limited types

  - Restricted in SPARK for access types (due to ownership policy)

* Evaluation for the copy may raise runtime errors

  - Not allowed by default inside *potentially unevaluated expressions*

    .. code:: Ada

       procedure Extract (A : in out My_Array;
                          J : Integer;
                          V : out Value)
         with Post =>
           (if J in A'Range then V = A(J)'Old); -- Illegal

  - Use `pragma Unevaluated_Use_Of_Old (Allow)` to allow

----------------------------------
Special Cases for Attribute 'Old
----------------------------------

* Function call in the prefix of `'Old` is evaluated at subprogram entry

  - Value of globals is the one at subprogram entry

  - Not the same as calling the function on parameters with `'Old`

    .. code:: Ada

       function F (X : Integer) return Integer
         with Global => Glob;

       procedure P (X : in out Integer)
         with Post =>
           F (X'Old) = 0 and then
           F (X)'Old = 0;

* Prefix of access type needs to be a call to an *allocating function*

  .. code:: Ada

     function Copy (X : Ptr) return Ptr
       with Post => Copy'Result.all = Ptr.all;

     procedure P (X : in out Ptr)
       with Post => Property (Copy (X)'Old);

------------------------------------
New Expressions For Specifications
------------------------------------

* *if expressions* express a logical implication

  - `(if A then B)` is the same as "A implies B" or "(not A) or B"

  - `(if A then B else C)` is "(A implies B) and ((not A) implies C)"

  - complete form has `elsif` parts

  - if-then-else form can be used with arbitrary types:

    .. code:: Ada

       A := (if X then 2 else 3);

* *case expressions* is the extension to non-boolean discrete types

--------------
Set Notation
--------------

* Usable in both *case expressions* / *case statements* and in membership tests

* Without set notation:

  .. code:: Ada

     if X = 'A' or else X = 'B' or else X = 'C' then

* With set notation:

  .. code:: Ada

     if X in 'A' | 'B' | 'C' then

* Also allowed for opposite membership test: `if X not in ...`

---------------------
Declare Expressions
---------------------

* New feature in Ada 2022

* Convenient shorthand for repeated subexpression

  - Only constants and renamings allowed
  - Typically used in postconditions

  .. code:: Ada

     function Find (T : A; R : Integer) return Integer
       with Post =>
         (declare
            Res : constant Integer := Find'Result;
          begin
            Res >= 0 and then
            (if Res /= 0 then T (Res) = R));

------------------
Delta Aggregates
------------------

* Express the value of a modified composite object (record or array)

  .. code:: Ada

     (Rec with delta Comp1 => Val1, Comp2 => Val2)

     (Arr with delta 1 => True, 42 => False)

* Typically used to relate input and output values of parameters

  - Combines delta aggregate with use of attribute `'Old`

  .. code:: Ada

     procedure P (Rec : in out T)
        with Post => Rec = (Rec'Old with delta Comp1 => Val1,
                                               Comp2 => Val2);

* With array object:

  - Avoids the introduction of explicit quantifiers
  - Can have overlapping and dynamic choices (values or ranges)

----------------------
Expression Functions
----------------------

* Simple query functions used in contracts can be given as *expression
  functions*

  .. code:: Ada

     function Increment (X : Integer) return Integer is
       (X + 1);

* Above is equivalent to having a postcondition

  - But no subprogram body to add in the body unit

  .. code:: Ada

     function Increment (X : Integer) return Integer
       with Post => Increment'Result = X + 1;

* Precondition can be specified after the expression

  .. code:: Ada

     function Increment (X : Integer) return Integer is
       (X + 1)
       with Pre => X < Integer'Last;

-----------------------------
Use of Expression Functions
-----------------------------

* Expression functions can be declared in a package spec and used in contracts

* For queries over objects of a private type

  - Function spec is declared in the public part

  - Expression function is declared in the private part

  .. code:: Ada

     package P is
       type T is private;
       function Value (X : T) return Integer;
     private
       type T is new Integer;
       function Value (X : T) return Integer is (Integer (X));
     end;

  - :toolname:`GNATprove` uses the implicit postcondition to prove client units

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
