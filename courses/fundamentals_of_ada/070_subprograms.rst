*************
Subprograms
*************

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

--------------
Introduction
--------------

* Are syntactically distinguished as :ada:`function` and :ada:`procedure`

   - Functions represent *values*
   - Procedures represent *actions*

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean
      procedure Split (T : in out Tree;
                       Left : out Tree;
                       Right : out Tree)

* Provide direct syntactic support for separation of specification from implementation

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean;
      function Is_Leaf (T : Tree) return Boolean is
      begin
      ...
      end Is_Leaf;

--------------------------------------
Recognizing Procedures and Functions
--------------------------------------

* Functions' results must be treated as values

   - And cannot be ignored

* Procedures cannot be treated as values
* You can always distinguish them via the call context

   .. code:: Ada

      Open (Source, "SomeFile.txt");
      while not End_of_File (Source) loop
        Get (Next_Char, From => Source);
        if Found (Next_Char, Within => Buffer) then
          Display (Next_Char);
        end if;
      end loop;

----------------------------------
A Little "Preaching" About Names
----------------------------------

* Procedures are abstractions for actions
* Functions are abstractions for values
* Use names that reflect those facts!

   - Imperative verbs for procedure names
   - Nouns for function names, as for mathematical functions

      + Questions work for boolean functions

.. code:: Ada

   procedure Open (V : in out Valve);
   procedure Close (V : in out Valve);
   function Square_Root (V: Real) return Real;
   function Is_Open (V: Valve) return Boolean;

========
Syntax
========

-------------------------
 Specification and Body
-------------------------

* Subprogram specification is the external (user) **interface**

   - **Declaration** and **specification** are used synonymously

* Specification may be required in some cases

   - eg. recursion

* Subprogram body is the **implementation**

-------------------------------------------
Procedure Specification Syntax (Simplified)
-------------------------------------------

.. code:: Ada

   procedure Swap (A, B : in out Integer);

.. code:: Ada

   procedure_specification ::=
      procedure program_unit_name
        ( parameter_specification
        { ; parameter_specification} );

   parameter_specification ::=
      identifier_list : mode subtype_mark [ := expression ]

   mode ::= [in] | out | in out

------------------------------------------
Function Specification Syntax (Simplified)
------------------------------------------

.. code:: Ada

   function F (X : Real) return Real;

* Close to :ada:`procedure` specification syntax

  + With :ada:`return`
  + Can be an operator: :ada:`+ - * / mod rem` ...

.. container:: latex_environment footnotesize

   .. code:: Ada

      function_specification ::=
         function designator
           ( parameter_specification
           { ; parameter_specification} )
           return result_type;

      designator ::= program_unit_name | operator_symbol

-------------
Body Syntax
-------------

.. code:: Ada

   subprogram_specification is
      [declarations]
   begin
      sequence_of_statements
   end [designator];

.. code:: Ada

   procedure Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello World!");
      Ada.Text_IO.New_Line (2);
   end Hello;

   function F (X : Real) return Real is
      Y : constant Real := X + 3.0;
   begin
      return X * Y;
   end F;

--------------
Completions
--------------

* Bodies **complete** the specification

   - There are **other** ways to complete

* Separate specification is **not required**

   - Body can act as a specification

* A declaration and its body must **fully** conform

   - Mostly **semantic** check
   - But parameters **must** have same name

.. code:: Ada

   procedure P (J, K : Integer)
   procedure P (J : Integer; K : Integer)
   procedure P (J, K : in Integer)
   -- Invalid
   procedure P (A : Integer; B : Integer)

---------------------
Completion Examples
---------------------

* Specifications

   .. code:: Ada

      procedure Swap (A, B : in out Integer);
      function Min (X, Y : Person) return Person;

* Completions

   .. code:: Ada

      procedure Swap (A, B : in out Integer) is
        Temp : Integer := A;
      begin
        A := B;
        B := Temp;
      end Swap;

      function Mac (A, X, Y : Integer) return Integer is
      begin
         return A + X * Y;
      end Min;

------------------------------------------
Direct Recursion - No Declaration Needed
------------------------------------------

* When:ada:`is` is reached, the subprogram becomes **visible**

    - It can call **itself** without a declaration

.. code:: Ada

   type List is array (Natural range <>) of Integer;
   Empty_List : constant List (1 .. 0) := (others => 0);

   function Get_List return List is
     Next : Integer;
   begin
     Get (Next);

     if Next = 0 then
       return Empty_List;
     else
       return Get_List & Next;
     end if;
   end Input;

----------------------------
Indirect Recursion Example
----------------------------

* Elaboration in **linear order**

.. code:: Ada

   procedure P;

   procedure F is
   begin
     P;
   end F;

   procedure P is
   begin
     F;
   end P;

------
Quiz
------

Which profile is semantically different from the others?

   A. ``procedure P ( A : Integer; B : Integer );``
   B. ``procedure P ( A, B : Integer );``
   C. :answermono:`procedure P ( B : Integer; A : Integer );`
   D. ``procedure P ( A : in Integer; B : in Integer );``

.. container:: animate

   Parameter names are important in Ada.  The other selections have
   the names in the same order with the same mode and type.

============
Parameters
============

----------
Examples
----------

.. include:: examples/070_subprograms/parameters.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/070_subprograms.html#parameters`

----------------------------------
Subprogram Parameter Terminology
----------------------------------

* *Actual* parameters are values passed to a call

   - Variables, constants, expressions

* *Formal* parameters are defined by specification

   - Receive the values passed from the actual parameters
   - Specify the types required of the actual parameters
   - Type **cannot** be anonymous

   .. code:: Ada

      procedure Something (Formal1 : in Integer);

      ActualX : Integer;
      ...
      Something (ActualX);

---------------------------------
Parameter Associations In Calls
---------------------------------

* Associate formal parameters with actuals
* Both positional and named association allowed

.. code:: Ada

   Something (ActualX, Formal2 => ActualY);
   Something (Formal2 => ActualY, Formal1 => ActualX);

---------------------------------------
Actual Parameters Respect Constraints
---------------------------------------

* Must satisfy any constraints of formal parameters
* :ada:`Constraint_Error` otherwise

.. code:: Ada

   declare
     Q : Integer := ...
     P : Positive := ...
     procedure Foo (This : Positive);
   begin
     Foo (Q); -- runtime error if Q <= 0
     Foo (P);

-----------------
Parameter Modes
-----------------

* Mode :ada:`in`

   - Actual parameter is :ada:`constant`
   - Can have default, used when **no value** is provided

* Mode :ada:`out`

   - Writing is **expected**
   - Reading is **allowed**
   - Actual **must** be a writable object

* Mode :ada:`in out`

   - Actual is expected to be **both** read and written
   - Actual **must** be a writable object

* Function :ada:`return`

   - **Must** always be handled

---------------------------------
Why Read Mode `out` Parameters?
---------------------------------

* **Convenience** of writing the body

   - No need for readable temporary variable

* Warning: initial value is **not defined**

.. code:: Ada

   procedure Compute (Value : out Integer) is
   begin
     Value := 0;
     for K in 1 .. 10 loop
       Value := Value + K; -- this is a read AND a write
     end loop;
   end Compute;

---------------------------------
Skipping Over Actual Parameters
---------------------------------

* Requires named format for remaining arguments

.. code:: Ada

   procedure Activate (
     Process : in Process_Name := Unknown;
     Before : in Duration := 0.0;
     After : in Duration := 0.0);
   ...
   begin
     -- Parameter "Process" is skipped
     Activate (Before => 60.0, After => 10.0);
     Activate (60.0, 60.0); -- compile error
     -- Parameter "Before" is skipped
     Activate (My_Process, After => 10.0);
     Activate (My_Process, 60.0); -- Not an error, but WRONG!

.. container:: speakernote

   Not using named association can cause confusion if future development adds parameters

------------------------------
Parameter Passing Mechanisms
------------------------------

* Passed either "by-copy" or "by-reference"
* By-Copy

   - The formal denotes a separate object from the actual
   - A copy of the actual is placed into the formal before the call
   - A copy of the formal is placed back into the actual after the call

* By-Reference

   - The formal denotes a view of the actual
   - Reads and updates to the formal directly affect the actual

* Parameter **types** control mechanism selection

   - Not the parameter **modes**
   - Compiler determines the mechanism

-----------------------------------
Why Pass Parameters By-Reference?
-----------------------------------

* More efficient for large objects

   - The address of the actual is copied, rather than the value

* Little gain for small objects

   - When an address is about the same size

---------------
By-Copy Types
---------------

* Elementary types

   - Scalar
   - Access

* Private types if fully defined as elementary types

   - Described later

--------------------
By-Reference Types
--------------------

* :ada:`tagged` types
* :ada:`task` types and :ada:`protected` types
* :ada:`limited` types

   - Directly limited record types and their descendants
   - Not just those that are :ada:`limited private`
   - Described later

* Composite types with by-reference component types
* :ada:`private` types if fully defined as by-reference types

   - Described later

--------------------------------
Implementation-Dependent Types
--------------------------------

* :ada:`array` types containing only by-copy components
* Non-limited record types containing only by-copy components
* Implementation chooses most efficient method

   - Based on size of actual value to be passed
   - No gain if the value size approximates the size of an address

.. container:: speakernote

   So arrays of integers, or records of Booleans, etc

---------------------------------
Unconstrained Formal Parameters
---------------------------------

* Take bounds from **actual** parameters
* Also applies to formal :ada:`return` specification

    + Actual :ada:`return` is constrained in the body

.. code:: Ada

   type Vector is array (Positive range <>) of Real;
   procedure Print (Formal : Vector);

   Phase : Vector (X .. Y);
   State : Vector (1 .. 4);
   ...
   begin
     Print (Phase);          -- Formal'Range is X .. Y
     Print (State);          -- Formal'Range is 1 .. 4
     Print (State (3 .. 4)); -- Formal'Range is 3 .. 4

-----------------------------------
Unconstrained Parameters Surprise
-----------------------------------

* Assumptions about formal bounds may be **wrong**

.. code:: Ada

  type Vector is array (Positive range <>) of Real;
  function Subtract (Left, Right : Vector) return Vector;

  V1 : Vector (1 .. 10); -- length = 10
  V2 : Vector (15 .. 24); -- length = 10
  R : Vector (1 .. 10); -- length = 10
  ...
  -- What are the indices returned by Subtract?
  R := Subtract (V2, V1);

----------------------
Naive Implementation
----------------------

* **Assumes** bounds are the same everywhere
* Fails when :ada:`Left'First /= Right'First`
* Fails when :ada:`Left'First /= 1`

  .. code:: Ada

   function Subtract (Left, Right : Vector)
     return Vector is
      Result : Vector (1 .. Left'Length);
   begin
      ...
      for K in Result'Range loop
        Result (K) := Left (K) - Right (K);
      end loop;

------------------------
Correct Implementation
------------------------

* Covers **all** bounds
* :ada:`return` indexed by :ada:`Left'Range`

.. code:: Ada

   function Subtract (Left, Right : Vector) return Vector is
      Result : Vector (Left'Range);
      Offset : constant Integer := Right'First - Result'First;
   begin
      ...
      for K in Result'Range loop
        Result (K) := Left (K) - Right (K + Offset);
      end loop;

------
Quiz
------

.. code:: Ada

   function F (P1 : in     Integer   := 0;
               P2 : in out Integer;
               P3 : in     Character := ' ';
               P4 :    out Character)
      return Integer;
   J1, J2 : Integer;
   C : Character;

Which call is legal?

   A. ``J1 := F (P1 => 1, P2 => J2, P3 => '3', P4 => '4');``
   B. ``J1 := F (P1 => 1, P3 => '3', P4 => C);``
   C. :answermono:`J1 := F (1, J2, '3', C);`
   D. ``F (J1, J2, '3', C);``

.. container:: animate

   Explanations

   A. :ada:`P4` is :ada:`out`, it **must** be a variable
   B. :ada:`P2` has no default value, it **must** be specified
   C. Correct
   D. :ada:`F` is a function, its :ada:`return` **must** be handled

=================
Null Procedures
=================

-----------------------------
Null Procedure Declarations
-----------------------------

.. admonition:: Language Variant

   Ada 2005

* Shorthand for a procedure body that does nothing
* Longhand form

   .. code:: Ada

      procedure NOP is
      begin
        null;
      end NOP;

* Shorthand form

   .. code:: Ada

      procedure NOP is null;

* The :ada:`null` statement is present in both cases
* Explicitly indicates nothing to be done, rather than an accidental removal of statements

--------------------------------
Null Procedures As Completions
--------------------------------

.. admonition:: Language Variant

   Ada 2005

* Completions for a distinct, prior declaration

   .. code:: Ada

      procedure NOP;
      ...
      procedure NOP is null;

* A declaration and completion together

   - A body is then not required, thus not allowed

   .. code:: Ada

      procedure NOP is null;
      ...
      procedure NOP is -- compile error
      begin
        null;
      end NOP;

--------------------------------------
Typical Use for Null Procedures: OOP
--------------------------------------

.. admonition:: Language Variant

   Ada 2005

* When you want a method to be concrete, rather than abstract, but don't have anything for it to do

   - The method is then always callable, including places where an abstract routine would not be callable
   - More convenient than full null-body definition

------------------------
Null Procedure Summary
------------------------

.. admonition:: Language Variant

   Ada 2005

* Allowed where you can have a full body

   - Syntax is then for shorthand for a full null-bodied procedure

* Allowed where you can have a declaration!

   - Example: package declarations
   - Syntax is shorthand for both declaration and completion

      + Thus no body required/allowed

* Formal parameters are allowed

.. code:: Ada

   procedure Do_Something ( P : in     integer ) is null;

=====================
Nested Subprograms
=====================

--------------------------------
Subprograms within Subprograms
--------------------------------

* Subprograms can be placed in any declarative block

   * So they can be nested inside another subprogram
   * Or even within a :ada:`declare` block

* Useful for performing sub-operations without passing parameter data

----------------------------
Nested Subprogram Example
----------------------------

.. code:: Ada
   :number-lines: 1

   procedure Main is

      function Read (Prompt : String) return Types.Line_T is
      begin
         Put ("> ");
         return Types.Line_T'Value (Get_Line);
      end Read;

      Lines : Types.Lines_T (1 .. 10);
   begin
      for J in Lines'Range loop
         Lines (J) := Read ("Line " & J'Image);
      end loop;

=====================
Procedure Specifics
=====================

-----------------------------------
`Return` Statements In Procedures
-----------------------------------

.. container:: columns

 .. container:: column

    * Returns immediately to caller
    * Optional

       - Automatic at end of body execution

    * Fewer is traditionally considered better

 .. container:: column

    .. code:: Ada

       procedure P is
       begin
         ...
         if Some_Condition then
           return; -- early return
         end if;
         ...
       end P; -- automatic return

====================
Function Specifics
====================

--------------------------------
Return Statements In Functions
--------------------------------

* Must have at least one

   - Compile-time error otherwise
   - Unless doing machine-code insertions

* Returns a value of the specified (sub)type
* Syntax

   .. code:: Ada

      function defining_designator [formal_part]
           return subtype_mark is
        declarative_part
        begin
           {statements}
           return expression;
        end designator;

---------------------------------------
No Path Analysis Required By Compiler
---------------------------------------

* Running to the end of a function without hitting a :ada:`return` statement raises :ada:`Program_Error`
* Compilers can issue warning if they suspect that a :ada:`return` statement will not be hit

.. code:: Ada

   function Greater (X, Y : Integer) return Boolean is
   begin
     if X > Y then
       return True;
     end if;
   end Greater; -- possible compile warning

----------------------------
Multiple Return Statements
----------------------------

* Allowed
* Sometimes the most clear

.. code:: Ada

   function Truncated (R : Real) return Integer is
     Converted : Integer := Integer (R);
   begin
     if R - Real (Converted) < 0.0 then -- rounded up
       return Converted - 1;
     else -- rounded down
       return Converted;
     end if;
   end Truncated;

---------------------------------------
Multiple Return Statements Versus One
---------------------------------------

* Many can detract from readability
* Can usually be avoided

.. code:: Ada

   function Truncated (R : Real) return Integer is
     Result : Integer := Integer (R);
   begin
     if R - Real (Result) < 0.0 then -- rounded up
       Result := Result - 1;
     end if;
     return Result;
   end Truncated;

--------------------------------
Composite Result Types Allowed
--------------------------------

.. code:: Ada

   function Identity (Order : Positive := 3) return Matrix is
     Result : Matrix (1 .. Order, 1 .. Order);
   begin
     for K in 1 .. Order loop
       for J in 1 .. Order loop
         if K = J then
           Result (K,J) := 1.0;
         else
           Result (K,J) := 0.0;
         end if;
       end loop;
     end loop;
     return Result;
   end Identity;

-------------------------------
Function Dynamic-Size Results
-------------------------------

.. code:: Ada

 is
    function Char_Mult (C : Character; L : Natural)
      return String is
       R : String (1 .. L) := (others => C);
    begin
       return R;
    end Char_Mult;

    X : String := Char_Mult ('x', 4);
 begin
    -- OK
    pragma Assert (X'Length = 4 and X = "xxxx");

======================
Expression Functions
======================

----------
Examples
----------

.. include:: examples/070_subprograms/expression_functions.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/070_subprograms.html#expression-functions`

----------------------
Expression Functions
----------------------

.. admonition:: Language Variant

   Ada 2012

* Functions whose implementations are pure expressions

   - No other completion is allowed
   - No :ada:`return` keyword

* May exist only for sake of pre/postconditions

.. code:: Ada

   function function_specification is ( expression );

NB: Parentheses around expression are **required**

* Can complete a prior declaration

.. code:: Ada

   function Squared (X : Integer) return Integer;
   function Squared (X : Integer) return Integer is
      (X ** 2);

------------------------------
Expression Functions Example
------------------------------

.. admonition:: Language Variant

   Ada 2012

* Expression function

.. code:: Ada

   function Square (X : Integer) return Integer is (X ** 2);

* Is equivalent to

.. code:: Ada

   function Square (X : Integer) return Integer is
   begin
      return X ** 2;
   end Square;

------
Quiz
------

Which statement is True?

   A. Expression functions cannot be nested functions.
   B. Expression functions require a specification and a body.
   C. Expression functions must have at least one "return" statement.
   D. :answer:`Expression functions can have "out" parameters.`

.. container:: animate

   Explanations

   A. False, they can be declared just like regular function
   B. False, an expression function cannot have a body
   C. False, expression functions cannot contain a no :ada:`return`
   D. Correct, but it can assign to :ada:`out` parameters only by calling another function.

====================
Potential Pitfalls
====================

----------
Examples
----------

.. include:: examples/070_subprograms/potential_pitfalls.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/070_subprograms.html#potential-pitfalls`

-----------------------------
Mode `out` Risk for Scalars
-----------------------------

* Always assign value to :ada:`out` parameters
* Else "By-copy" mechanism will copy something back

   - May be junk
   - :ada:`Constraint_Error` or unknown behaviour further down

.. code:: Ada

   procedure P
     (A, B : in Some_Type; Result : out Scalar_Type) is
   begin
     if Some_Condition then
       return;  -- Result not set
     end if;
     ...
     Result := Some_Value;
   end P;

----------------
"Side Effects"
----------------

* Any effect upon external objects or external environment

   - Typically alteration of non-local variables or states
   - Can cause hard-to-debug errors
   - Not legal for :ada:`function` in SPARK

* Can be there for historical reasons

   - Or some design patterns

.. code:: Ada

   Global : Integer := 0;

   function F (X : Integer) return Integer is
   begin
      Global := Global + X;
      return Global;
   end P;

---------------------------------------
Order-Dependent Code And Side Effects
---------------------------------------

.. |rightarrow| replace:: :math:`\rightarrow`

.. code:: Ada

   Global : Integer := 0;

   function Inc return Integer is
   begin
     Global := Global + 1;
     return Global;
   end F;

   procedure Assert_Equals (X, Y : in Integer);
   ...
   Assert_Equals (Global, Inc);

* Language does **not** specify parameters' order of evaluation
* :ada:`Assert_Equals` could get called with

   - X |rightarrow| 0, Y |rightarrow| 1 (if :ada:`Global` evaluated first)
   - X |rightarrow| 1, Y |rightarrow| 1 (if :ada:`Inc` evaluated first)

--------------------
Parameter Aliasing
--------------------

* **Aliasing** : Multiple names for an actual parameter inside a subprogram body
* Possible causes:

   - Global object used is also passed as actual parameter
   - Same actual passed to more than one formal
   - Overlapping :ada:`array` slices
   - One actual is a component of another actual

* Can lead to code dependent on parameter-passing mechanism
* Ada detects some cases and raises :ada:`Program_Error`

.. code:: Ada

   procedure Update (Doubled, Tripled : in out Integer);
   ...
   Update (Doubled => A,
           Tripled => A);  -- illegal in Ada 2012

----------------------------
Functions' Parameter Modes
----------------------------

.. admonition:: Language Variant

   Ada 2012

* Can be mode :ada:`in out` and :ada:`out` too
* **Note:** operator functions can only have mode :ada:`in`

   - Including those you overload
   - Keeps readers sane

* Justification for only mode :ada:`in` prior to Ada 2012

   - No side effects: should be like mathematical functions
   - But side effects are still possible via globals
   - So worst possible case: side effects are possible and necessarily hidden!

----------------------------------
Easy Cases Detected and Not Legal
----------------------------------

.. code:: Ada

   function Increment (This : in out Integer) return Integer is
   begin
      This := This + 1;
      return This;
   end Increment;

   X : array (1 .. 10) of Integer;
   ...
   -- order of evaluating A not specified
   X (A) := Increment (A); -- not legal in Ada 2012

===================
Extended Examples
===================

------------------------------------
Tic-Tac-Toe Winners Example (Spec)
------------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       package TicTacToe is
         type Players is (Nobody, X, O);
         type Move is range 1 .. 9;
         type Game is array (Move) of
           Players;
         function Winner (This : Game)
           return Players;
         ...
       end TicTacToe;

 .. container:: column

    .. list-table::

      * - :subscript:`1` N

        - :subscript:`2` N
        - :subscript:`3` N

      * - :subscript:`4` N

        - :subscript:`5` N
        - :subscript:`6` N

      * - :subscript:`7` N

        - :subscript:`8` N
        - :subscript:`9` N

.. container:: speakernote

   Prior to Ada2012 use:
   type Game is record
     Board : Moves := (others ``=>`` Nobody);
   end record;

------------------------------------
Tic-Tac-Toe Winners Example (Body)
------------------------------------

.. code:: Ada

   function Winner (This : Game) return Players is
     type Winning_Combinations is range 1 .. 8;
     type Required_Positions   is range 1 .. 3;
     Winning : constant array
       (Winning_Combinations, Required_Positions)
         of Move := (-- rows
                     (1, 2, 3), (4, 5, 6), (7, 8, 9),
                     -- columns
                     (1, 4, 7), (2, 5, 8), (3, 6, 9),
                     -- diagonals
                     (1, 5, 9), (3, 5, 7));

   begin
     for K in Winning_Combinations loop
       if This (Winning (K, 1)) /= Nobody and then
         (This (Winning (K, 1)) = This (Winning (K, 2)) and
          This (Winning (K, 2)) = This (Winning (K, 3)))
       then
         return This (Winning (K, 1));
       end if;
     end loop;
     return Nobody;
   end Winner;

-------------
Set Example
-------------

.. code:: Ada

   -- some colors
   type Color is (Red, Orange, Yellow, Green, Blue, Violet);
   -- truth table for each color
   type Set is array (Color) of Boolean;
   -- unconstrained array of colors
   type Set_Literal is array (Positive range <>) of Color;

   -- Take an array of colors and set table value to True
   -- for each color in the array
   function Make (Values : Set_Literal) return Set;
   -- Take a color and return table with color value set to true
   function Make (Base : Color) return Set;
   -- Return True if the color has the truth value set
   function Is_Member (C : Color; Of_Set: Set) return Boolean;

   Null_Set : constant Set := (Set'Range => False);
   RGB      : Set := Make (
              Set_Literal'( Red, Blue, Green));
   Domain   : Set := Make (Green);

   if Is_Member (Red, Of_Set => RGB) then ...

   -- Type supports operations via Boolean operations,
   -- as Set is a one-dimensional array of Boolean
   S1, S2 : Set := Make (....);
   Union : Set := S1 or S2;
   Intersection : Set := S1 and S2;
   Difference : Set := S1 xor S2;

------------------------------
Set Example (Implementation)
------------------------------

.. code:: Ada

   function Make (Base : Color) return Set is
     Result : Set := Null_Set;
   begin
      Result (Base) := True;
      return Result;
   end Make;

   function Make (Values : Set_Literal) return Set is
     Result : Set := Null_Set;
   begin
     for K in Values'Range loop
       Result (Values (K)) := True;
     end loop;
     return Result;
   end Make;

   function Is_Member ( C: Color;
                        Of_Set: Set)
                        return Boolean is
   begin
     return Of_Set(C);
   end Is_Member;

========
Lab
========

.. include:: labs/070_subprograms.lab.rst

=========
Summary
=========

---------
Summary
---------

* :ada:`procedure` is abstraction for actions
* :ada:`function` is abstraction for value computations
* A :ada:`function` may return values of variable size
* Separate declarations are sometimes necessary

   - Mutual recursion
   - Visibility from packages (i.e., exporting)

* Modes allow spec to define effects on actuals

   - Don't have to see the implementation: abstraction maintained

* Parameter-passing mechanism is based on the type
* Watch those side effects!
