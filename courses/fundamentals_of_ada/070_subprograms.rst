
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

* Are syntactically distinguished as `function` and `procedure`

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
 
========================
Declarations and Bodies
========================

----------
Examples
----------

.. include:: examples/070_subprograms/declarations_and_bodies.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/070_subprograms.html#declarations-and-bodies`

--------------------------
Subprogram Declarations 
--------------------------

* Define the external (user) interface

   - Provide the subprogram specification
   - *Declaration* and *specification* are often used synonymously

* Required in some circumstances

   - You'll see them...

* Optional in other circumstances

.. code:: Ada

   procedure Swap (A, B : in out Integer);
   function F (X : Real) return Real;
 
-------------------
Subprogram Bodies
-------------------

* Provide the implementation
* Define execution behavior

   - Procedure body

      .. code:: Ada

         procedure Swap (A, B : in out Integer) is
           Temp : Integer := A;
         begin
           A := B;
           B := Temp;
         end Swap;
 
   - Function body

      .. code:: Ada

         function F (X : Real) return Real is
         begin
           return X + 3.0 * X;
         end F;
 
-------------------------------------------
Procedure Declaration Syntax (Simplified)
-------------------------------------------

.. code:: Ada

   subprogram_declaration ::= subprogram_specification ; 
   subprogram_specification ::=
      procedure defining_name parameter_profile  
   parameter_profile ::= [ formal_part ]
   formal_part ::=
      ( parameter_specification
        { ; parameter_specification } )
   parameter_specification ::=
      defining_identifier_list : mode subtype_mark
         [ := expression ]
   mode ::= [in] | out | in out
 
------------------------------------------
Function Declaration Syntax (Simplified)
------------------------------------------

.. code:: Ada

   subprogram_declaration ::= subprogram_specification;
   subprogram_specification ::= function
       defining_designator parameter_and_result_profile
   defining_designator ::= defining_program_unit_name | 
                           defining_operator_symbol
   operator_symbol ::= string_literal
   parameter_and_result_profile ::=
      [formal_part] return subtype_mark
 
* (remainder same as procedures)

-------------
Body Syntax
-------------

.. code:: Ada
    
   subprogram_body ::= subprogram_specification is
                         {basic_declarative_item}
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
   begin
     return X + 3.0 * X;
   end F;
     
---------------
"Completions"
---------------

* Bodies "complete" the corresponding declarations

   - "Completions" because there are other ways to complete a declaration in addition to writing a full body

* Bodies may act as both declaration and completion

   - Separate declarations not required to define subprograms

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
      
      function Min (X, Y : Person) return Person is
      begin
        if X.Age < Y.Age then
          return X;
        else
          return Y;
        end if;
      end Min;
 
* Depending on usage, specifications may not be needed

------------------------
Completion Conformance
------------------------

* Profile conformance is an issue in several situations

   - Separate declaration in a package specification
   - Access types designating subprograms
   - Others...

* A declaration and its body must fully conform

   - So compiler can be certain which goes with which

* Syntax of profiles need not be identical as long as semantics are the same

   - But need not be *textually* identical

.. code:: Ada

   procedure P (J, K : Integer)
   procedure P (J : Integer; K: Integer)
   procedure P (J, K : in Integer)

----------------------------
Why Separate Declarations?
----------------------------

* Packages exporting subprograms

   - Package declarations never contain bodies of anything
   - Explained when we cover packages...

* Recursion

   - Subprograms may call themselves 

      + Directly 

      + Indirectly

   - Limited only by available memory

--------------------------
Direct Recursion Example
--------------------------

.. code:: Ada

   type List is array (Natural range <>) of Integer;
   Empty_List : constant List (1 .. 0) := (others => 0);
   function Input return List is
     Next : Integer;
   begin
     Put ("Enter an integer, or 0 to quit:");
     Get (Next);
     if Next = 0 then
       return Empty_List;
     else
       return Next & Input;
     end if;
   end Input;
 
----------------------------------------
Indirect Recursion Needs A Declaration
----------------------------------------

.. container:: columns

 .. container:: column
  
    * Due to linear elaboration order
    * Only one of the two need be declared separately

 .. container:: column
  
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

* Specification

   .. code:: Ada

      procedure Something ( Formal1 : in     Integer;
                            Formal2 :    out Boolean );
 
* Call

   .. code:: Ada

      ActualX : Integer;
      ActualY : Boolean;
      ...
      Something ( ActualX, ActualY );
 
---------------------------------
Parameter Associations In Calls
---------------------------------

* Associate formal parameters with actuals
* Traditional "positional association" is allowed

   - Nth actual goes to nth formal

* "Named association" also allowed

   - Name of formal parameter is repeated
   - Order of associations may be altered

.. code:: Ada

   Something ( ActualX, Formal2 => ActualY );
   Something (Formal2 => ActualY, Formal1 => ActualX);

---------------------------------------
Actual Parameters Respect Constraints
---------------------------------------

* Must satisfy any constraints of formal parameters
* `Constraint_Error` otherwise

.. code:: Ada

   declare
     Q : Integer := ...
     P : Positive := ...
     procedure Foo (This : Positive);
   begin
     Foo (Q); -- runtime error if Q <= 0
     Foo (P);

--------------------------------------------
No `subtype_indications` In Specifications
--------------------------------------------

.. code:: Ada

   subtype_mark <constraint>
 
* Obviates pathology regarding dynamic subtypes
* Illegal usage

   .. code:: Ada

      Lower, Upper : Integer;
      procedure P (X : in Integer range Lower .. Upper );
      -- code which affects Lower and/or Upper...
      procedure P (X : in Integer range Lower .. Upper )  is 
      begin
      ... 
      end P;
 
-----------------------
Use Named Constraints
-----------------------

* Use subtypes instead of `subtype_indications`
* Legal usage

   .. code:: Ada

      Lower, Upper : Integer;
      ...
      subtype Short is range Lower .. Upper;
      -- definition frozen - cannot change
      procedure P (X : in Short );
      -- code which affects Lower and/or Upper...
      -- "Short" does not change
      procedure P (X : in Short ) is 
      begin
      ... 
      end P;
 
------------------------------
No Anonymously-Typed Formals
------------------------------

* No name to use in type checking for formals to actuals
* No name for type checking function results to target

   .. code:: Ada

      procedure P (Formal : in array (X .. Y) of Some_Type);
      function F return array (X .. Y) of Some_Type;
 
* Use named types instead of anonymous types

   .. code:: Ada

      type List is array  (X .. Y) of Integer;
      ...
      procedure P (Formal : in List);
      function F return List;
 
-----------------
Parameter Modes
-----------------

* Complete abstraction by presenting different views

   - Views within the subprogram with respect to formals

* Views control use of formals within subprograms
* Mode `in`

   - Specifies that actual parameter is not altered

   - Only reading of formal is allowed 

* Mode `out`

   - Writing is expected, but reading is also allowed
   - Initial value inside subprogram is not defined

* Mode `in out`

   - Actual is expected to be both read and altered
   - Initial value inside subprogram is defined (taken from actual)

---------------------------------
Why Read Mode `out` Parameters?
---------------------------------

* Convenience of writing the body

   - No need for readable temporary variable in place of formal

* Be aware that initial value is not defined

   - There is no input value (with some exceptions...)

.. code:: Ada

   procedure Compute (Value : out Integer) is
   begin
     Value := 0;
     for K in 1 .. 10 loop
       Value := Value + K; -- this is a read AND a write
     end loop;
   end Compute;
 
--------------------------
Parameter Modes' Benefit
--------------------------

* Callers need not examine the implementation to determine effect upon actuals
* Intended effect (or lack thereof) is in specification

   - Although weakly guaranteed

   .. code:: Ada

      Procedure Put ( X : in integer );
      Procedure Get ( X : out integer );
 
---------------------------------
Modes' Requirements for Actuals
---------------------------------

* Use of variables versus expressions for actuals
* Modes `in out` and `out`

   - Variables must be used since actual may/will be altered

* Mode `in`

   - Expressions may be used since the actual can't be altered
   - Recall expressions not limited to variable references

.. code:: Ada

   procedure Do_Something (X : in     Integer;
                           Y :    out Integer );
   ...
   begin
     Do_Something(X,Y);    -- legal
     Do_Something(X+2, Y); -- legal
     Do_Something(X, Y+1); -- compile error
 
-------------------------------------
Parameter Defaults May Be Specified
-------------------------------------

* Mode `in` formals only
* Callers may omit corresponding actual for calls

   - Possible since actual will not be altered

.. code:: Ada

   My_Process, Your_Process : Process_Name;
   Period : Duration;
   procedure Activate( Process : in Process_Name;
                       After : in Process_Name := None;
                       Wait : in Duration := 0.0;
                       Prior : in Boolean := False  );
   ...
   begin
     -- no defaults taken
     Activate (My_Process, Your_Process, Period, True);
     -- defaults for After, Wait, Prior
     Activate (My_Process);
     -- defaults for Wait, Prior
     Activate (My_Process, Your_Process);

---------------------------------
Skipping Over Actual Parameters
---------------------------------

* Requires named format for remaining arguments

.. code:: Ada

   procedure Activate(
     Process : in Process_Name;
     After : in Process_Name := None;
     Wait : in Duration := 0.0;
     Prior : in Boolean := False );
   ...
   begin
     -- Parameter "After" is skipped
     Activate (My_Process, Wait => 60.0, Prior => True);
     Activate (My_Process, 60.0, True); -- compile error
 
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

* `tagged` types
* `task` types and `protected` types

* `limited` types 

   - Directly limited record types and their descendants
   - Not just those that are `limited private`
   - Described later

* Composite types with by-reference component types
* `private` types if fully defined as by-reference types

   - Described later

--------------------------------
Implementation-Dependent Types
--------------------------------

* `array` types containing only by-copy components
* Non-limited record types containing only by-copy components
* Implementation chooses most efficient method

   - Based on size of actual value to be passed
   - No gain if the value size approximates the size of an address

.. container:: speakernote

   So arrays of integers, or records of Booleans, etc

---------------------------------
Unconstrained Formal Parameters
---------------------------------

* Take bounds from actual parameters

.. code:: Ada

   type Vector is array (Positive range <>) of Real;
   Phase : Vector (X .. Y);
   State : Vector (1 .. 4);
   procedure Print (V : in Vector) is
   begin
     for Index in V'Range loop
       Put (V (Index));
     end loop;
   end Print;
   ...
   begin
     Print (Phase);       -- V'range is X .. Y
     Print (State);       -- V'range is 1 .. 4
     Print (State(1..2)); -- V'range is 1 .. 2
   end;
 
-----------------------------------
Unconstrained Parameters Surprise
-----------------------------------

* Taking bounds from actual sometimes requires care
* Assumptions about bounds of formal may be wrong

.. code:: Ada

   procedure Test is
     type Vector is array (Positive range <>) of Real;
     function Subtract (Left, Right : Vector) return Vector is ...
     V1 : Vector (1 .. 10); -- length = 10
     V2 : Vector (15 .. 24); -- length = 10
     R : Vector (1 .. 10); -- length = 10
   begin
     ...
     -- What are the indices returned by Subtract?
     R := Subtract (V2, V1);
     ...
   end;
 
----------------------
Naive Implementation
----------------------

* Assumes bounds are the same everywhere

.. code:: Ada

   function Subtract (Left, Right : Vector) return Vector is
     -- either length will do
     Result : Vector (1 .. Left'Length);
   begin
     if Left'Length /= Right'Length then
       raise Length_Error;
     end if;
     for K in Result'Range loop
       Result (K) := Left (K) - Right (K);
     end loop;
     return Result;
   end Subtract;
 
.. container:: speakernote

  If Left and Right have different 'first and 'last, that's a problem

------------------------
Correct Implementation
------------------------

* Covers all bounds

.. code:: Ada

   function Subtract (Left, Right : Vector) return Vector is
     Result : Vector (Left'Range);
     Offset : Integer;
   begin
     if Left'Length /= Right'Length then
       raise Length_Error;
     end if;
     -- Offset will be positive, negative or zero
     Offset := Right'First - Result'First;
     for K in Result'Range loop
       Result (K) := Left (K) - Right (K + Offset);
     end loop;
     return Result;
   end Subtract;
 
------
Quiz
------

.. code:: Ada

   function F (P1 : in     Integer   := 0;
               P2 : in out Integer;
               P3 : in     Character := ' ';
               P4 :    out Character)
      return Integer;
   I1, I2, I3, I4 : Integer;
   C1, C2, C3, C4 : Character;

Which call is legal?

   A. ``I4 := F (P1 => 1, P2 => 2, P3 => '3', P4 => 4);``
   B. ``I4 := F (P1 => 1, P3 => C3, P4 => C4);``
   C. :answermono:`I4 := F (I1, I2, C3, C4);`
   D. ``F (I1, I2, C3, C4);``

.. container:: animate

   Explanations

   A. :ada:`P4` can be modified by :ada:`P`, so it must be a variable
   B. :ada:`P2` has no default value so it must be specified
   C. Correct
   D. :ada:`F` is a function - return value must be stored

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
 
* The `null` statement is present in both cases
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
   * Or even within a `declare` block

* Useful for performing sub-operations without passing parameter data

----------------------------
Nested Subprogram Example
----------------------------

.. code:: Ada

   procedure Main is

      function Read (Prompt : String) return Types.Line_T is
         function Read (Inner_Prompt : String) return Types.Coordinate_T is
         begin
            Put (Prompt & " - " & Inner_Prompt & "> ");
            return Types.Coordinate_T'Value (Get_Line);
         end Read;
      begin
         return (X => Read ("X coordinate"), Y => Read ("Y coordinate"));
      end Read;

      Count : Natural;

   begin
      Put ("Number of lines: ");
      Count := Natural'Value (Get_Line);
      declare
         Lines : Types.Lines_T (1 .. Count);
         procedure Print (I : Natural) is
         begin
            Put_Line (I'Image & " => ( " & Lines (I).X'Image & ", " & Lines (I).Y'Image & " )");
         end Print;
      begin
         for I in Lines'Range loop
            Lines (I) := Read ("Line " & I'Image);
         end loop;
         for I in Lines'First .. Lines'Last loop
            Print (I);
         end loop;
      end;

   end Main;

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

* Running to the end of a function without hitting a `return` statement raises `Program_Error`
* Compilers can issue warning if they suspect that a `return` statement will not be hit

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
 
----------------------------------------
Function Results Are Objects
----------------------------------------

.. code:: Ada

   type Record_T is record
      Field1 : String (1 .. 10);
      Field2 : Character;
   end record;
   function Return_Record (C : Character) return Record_T is
   begin
      return (Field1 => (others => C), Field2 => C);
   end Return_Record;
   function Return_String (C : Character; L : Natural) return String is
      R : String (1 .. L) := (others => C);
   begin
      return R;
   end Return_String;

   -- s set to 'field1' in returned record
   S : String := Return_Record (' ').Field1;
   -- c set to character at index 3 in returned string
   C : Character := Return_String ('x', 4) (3);
 
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

* Shorthand for declaring functions whose implementations are only "expressions"

   - Only the returned value appears

* Syntax
    
   .. code:: Ada
    
      expr_func_declaration ::= function_specification is (expression);
     
   * Parentheses are required
   * Parameters are optional, as usual, but typical

* Expression function
    
   .. code:: Ada
    
      function Square (X : Integer) return Integer is (X ** 2);
     
* Is equivalent to
    
   .. code:: Ada
    
      function Square (X : Integer) return Integer is
      begin
         return X ** 2;
      end Square;
     
-------------------------------------
Expression Functions As Completions
-------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Completions for a distinct, prior declaration

   .. code:: Ada

      function Squared (X : Integer) return Integer;
      function Squared (X : Integer) return Integer is
          (X ** 2);
 
* A declaration and completion together

   - A body is then not required, thus not allowed

      .. code:: Ada

         function Squared (X : Integer) return Integer is
          (X ** 2);
         -- Then this would be a compile error
         function Squared (X : Integer) return Integer is
         begin
           return X ** 2;
         end Squared;
 
---------------------------------------
Typical Uses for Expression Functions
---------------------------------------

.. admonition:: Language Variant

   Ada 2012

* May be part of general (ADT) implementation
* May exist only for sake of pre/postconditions

   - Convenience: likely to appear where bodies are not allowed
   - Can supply higher level of abstraction

.. code:: Ada

   procedure Euclid (A, B : Integer; Result : out Integer) 
     with Pre => A > 0 and B > 0,
     Post => GCD (A, B, Result);
   function GCD (A, B, Candidate : Integer)
       return Boolean is
     (A rem Candidate = 0 and 
      B rem Candidate = 0 and
      (for all K in 1 .. Integer'Min (A,B) => 
        (if (A rem K = 0 and B rem K = 0)
         then K <= Candidate)));
 
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

   A. Expression functions can be declared anywhere a regular function is declared.
   B. An expression function body can act as its own specification.
   C. Expression functions only contain an expression - no :ada:`return` statement allowed.
   D. Correct - although the expression function itself cannot modify an :ada:`out` parameter, it could call another function that does.

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

.. container:: columns

 .. container:: column
  
    * Be sure to assign value of mode `out` formals!
    * "By-copy" mechanism will copy something back

       - Value copied back may be junk
       - `Constraint_Error` may be raised later or some other unexpected behavior may occur

 .. container:: column
  
    .. code:: Ada
    
       procedure P (
          A, B : in Some_Type;  
          Result : out Scalar_Type)
       is
       begin
         Some_Statements;
         if Some_Condition then 
           return;  -- Result not set
         end if;
         Some_Statements;
         Result := Some_Value;
       end P;
     
----------------
"Side Effects"
----------------

* Any effect upon external objects or external environment

   - Typically alteration of non-local variables or states

   .. code:: Ada

      Global : Integer := 0;
      function F (X : Integer) return Integer is
      begin
        Global := Global + X;
      return X;
      end F;
 
   - Should generally be avoided!

      + They make nasty errors possible
      + Not legal in SPARK

   - Are not specific to Ada

-----------------------------
Side Effects' Justification
-----------------------------

* Functions could not update formal parameters prior to Ada 2012

  - Not without some ugly tricks...

* May be part of the most clear approach

   * So-called "memo" functions

      .. code:: Ada

         Counter : integer := 0;
         function Next_Available return Integer is
         begin
           Counter := Counter + 1
           return Counter;
         end Next_Available;
 
   * "Reasonable" side effect

      .. code:: Ada

         Seed : Integer;
         function Random_Number return Real is
           Result : Real;
         begin
           -- Compute result based on Seed
           -- Alter Seed so next call gets different value
           return Result;
         end Random_Number;

---------------------------------------
Order-Dependent Code And Side Effects
---------------------------------------
.. |rightarrow| replace:: :math:`\rightarrow`

.. code:: Ada
    
   Global : Integer := 0;
   function F return Integer is
   begin
     Global := Global + 1;
     return Global;
   end F;
   procedure Gear_Down (
      X, Y : in Integer) is
   begin
     if X = Y then -- put gear down
     ...
     end if;
   end Gear_Down;
   ...
   Gear_Down (Global, F);
     
* Order of evaluation of parameters in subprogram call is not specified in language
* `Gear_Down` could get called with

   - X |rightarrow| 0, Y |rightarrow| 1 (if `Global` evaluated first)
   - X |rightarrow| 1, Y |rightarrow| 1 (if `F` evaluated first)

--------------------
Parameter Aliasing
--------------------

* When there are multiple names for an actual parameter inside a subprogram body

   - Global variable passed as actual parameter and referenced inside subprogram via global name
   - Same actual passed to more than one formal
   - Two actuals are overlapping slices
   - One actual is contained within another actual

* Can lead to code dependent on parameter-passing mechanism

   - Issue is not specific to Ada!

* Ada detects some cases

   - When detected, raises `Program_Error`
   - When not detected, does whatever it does

----------------------------------------
Parameter Aliasing via Global Variable
----------------------------------------

.. code:: Ada

     Actual : String := "Hello";
     procedure Print (Formal : in String) is
     begin
       Actual := "World";
       -- output dependent on passing mechanism
       Put_Line (Formal);
     end Print;
   begin
     -- if pass by-copy, prints "Hello"
     -- if pass by-reference, prints "World"
     Print (Formal => Actual); 
 
-----------------------------------------
Parameter Aliasing via Multiple Actuals
-----------------------------------------

.. code:: Ada

     Actual : String := "Hello";
     procedure Print (Formal1 : out String;
                      Formal2 : in String) is
     begin
       Formal1 := "World";
       -- output dependent on passing mechanism
       Put_Line (Formal2);
     end Print;
   begin
     -- if pass by-copy, prints "Hello"
     -- if pass by-reference, prints "World"
     Print (Actual, Actual);
 
---------------------------------------
Easy Cases Detected and Not Legal (1)
---------------------------------------

.. code:: Ada

   -- order of copying data to actual params not specified
   procedure Update (Doubled, Tripled : in out Integer) is
   begin
     Doubled := Doubled * 2;
     Tripled := Tripled * 3;
   end Update;
   procedure Test is
     A : Integer := 1;
   begin
     Update (Doubled => A,
             Tripled => A);  -- illegal in Ada 2012
     -- Could print "2" or "3" depending on copy order
     Put_Line (A'Img);
   end Test;
 
.. container:: speakernote

   Ada 2012 - overlap is no longer allowed

----------------------------
Functions' Parameter Modes
----------------------------

.. admonition:: Language Variant

   Ada 2012

* Can be mode `in out` and `out` too
* **Note:** operator functions can only have mode `in`

   - Including those you overload
   - Keeps readers sane

* Justification for only mode `in` prior to Ada 2012

   - No side effects: should be like mathematical functions
   - But side effects are still possible via globals
   - So worst possible case: side effects are possible and necessarily hidden!
   - Technical issues too...

---------------------------------------
Easy Cases Detected and Not Legal (2)
---------------------------------------

.. code:: Ada

     X : array (1 .. 10) of Integer := (others => 42);
     function F (This : in out Integer) return Integer is
     begin
       This := This + 1;
       return This;
     end F;
     A : Integer := 1;
   begin
     -- order of evaluating A not specified
     X (A) := F (A); -- not legal in Ada 2012
     Put_Line ("X(1) is"  &  X(1)'Img); -- "2" or "42"
     Put_Line ("X(2) is"  &  X(2)'Img); -- "2" or "42"
 
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

* Procedures are abstractions for actions
* Functions are abstractions for value computations
* Functions may return composite values
* Separate declarations are sometimes necessary

   - Mutual recursion
   - Visibility from packages (i.e., exporting)

* Modes allow spec to define effects on actuals

   - Don't have to see the implementation: abstraction maintained

* Parameter-passing mechanism is based on the type
* Watch those side effects!
