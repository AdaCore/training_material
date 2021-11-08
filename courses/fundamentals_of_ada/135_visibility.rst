************
Visibility
************

.. include:: support_files/symbols.rst

==============
Introduction
==============

-----------------------
Improving Readability
-----------------------

* Descriptive names plus hierarchical packages makes for very long statements

   .. code::

      Messages.Queue.Diagnostics.Inject_Fault (
         Fault    => Messages.Queue.Diagnostics.CRC_Failure,
         Position => Messages.Queue.Front );

* Operators treated as functions defeat the purpose of overloading

   .. code::

      Complex1 := Complex_Types."+" ( Complex2, Complex3 );

* Ada has mechanisms to simplify hierarchies

--------------------------
Operators and Primitives
--------------------------

* *Operators*

   - Constructs which behave generally like functions but which differ syntactically or semantically.
   - Typically arithmetic, comparison, and logical

* **Primitive operation**

   - Predefined operations such as ``=`` and ``+``  etc.
   - Subprograms declared in the same package as the type and which operate on the type
   - Inherited or overridden subprograms
   - For :ada:`tagged` types, class-wide subprograms
   - Enumeration literals

===============
"use" Clauses
===============

----------
Examples
----------

.. include:: examples/135_visibility/use_clauses.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/135_visibility.html#use-clauses`

----------------
 `use` Clauses
----------------

* Provide direct visibility into packages' exported items

   + *Direct Visibility* - as if object was referenced from within package being used

* May still use expanded name

.. code:: Ada

   package Ada.Text_IO is
     procedure Put_Line( ... );
     procedure New_Line( ... );
     ...
   end Ada.Text_IO;

   with Ada.Text_IO;
   procedure Hello is
     use Ada.Text_IO;
   begin
     Put_Line( "Hello World" );
     New_Line(3);
     Ada.Text_IO.Put_Line ( "Good bye" );
   end Hello;

---------------------
`use` Clause Syntax
---------------------

* May have several, like :ada:`with` clauses
* Must name an imported library unit

   - From same context clause

* Syntax

   .. code:: Ada

      use_package_clause ::= use package_name {, package_name};

* Can only :ada:`use` a library package

   - Subprograms don't make sense

--------------------
`use` Clause Scope
--------------------

* Applies to end of body, from first occurrence

.. code:: Ada

   package Pkg_A is
      Constant_A : constant := 123;
   end Pkg_A;

   package Pkg_B is
      Constant_B : constant := 987;
   end Pkg_B;

   with Pkg_A;
   with Pkg_B;
   use Pkg_A; -- everything in Pkg_A is now visible
   package P is
      A  : Integer := Constant_A; -- legal
      B1 : Integer := Constant_B; -- illegal
      use Pkg_B; -- everything in Pkg_B is now visible
      B2 : Integer := Constant_B; -- legal
      function F return Integer;
   end P;

   package body P is
     -- all of Pkg_A and Pkg_B is visible here
     function F return Integer is ( Constant_A + Constant_B );
   end P;

--------------------
No Meaning Changes
--------------------

* A new :ada:`use` clause won't change a program's meaning!
* Any directly visible names still refer to the original entities

.. code:: Ada

   package D is
     T : Real;
   end D;

   with D;
   procedure P is
     procedure Q is
       T, X : Real;
     begin
       ...
       declare
         use D;
       begin
         -- With or without the clause, "T" means Q.T
         X := T;
       end;
       ...
     end Q;

---------------------------
No Ambiguity Introduction
---------------------------

.. code:: Ada

   package D is
     V : Boolean;
   end D;

   package E is
     V : Integer;
   end E;
   with D, E;

   procedure P is
     procedure Q is
       use D, E;
     begin
       -- to use V here, must specify D.V or E.V
       ...
     end Q;
   begin
   ...

.. container:: speakernote

   For declarations in different packages that would not be directly visible in the absence of a "use" clause, none with the same identifier will be directly visible in the presence of such a clause, unless both are overloadable (i.e., enumeration literals and subprogram declarations)

------------------------------
`use` Clauses and Child Units
------------------------------

* A clause for a child does **not** imply one for its parent
* A clause for a parent makes the child **directly** visible

   - Since children are 'inside' declarative region of parent

.. code:: Ada

   package Parent is
     P1 : Integer;
   end Parent;

   package Parent.Child is
     PC1 : Integer;
   end Parent.Child;

   with Parent.Child;
   procedure Demo is
     D1 : Integer := Parent.P1;
     D2 : Integer := Parent.Child.PC1;
     use Parent;
     D3 : Integer := P1;
     D4 : Integer := Child.PC1;
     ...

.. container:: speakernote

   D4 has access to CHILD because PARENT is "use"d

----------------------------------------
`use` Clause and Implicit Declarations
----------------------------------------

* Visibility rules apply to implicit declarations too

.. code:: Ada

   package P is
     type Int is range Lower .. Upper;
     -- implicit declarations
     -- function "+"( Left, Right : Int ) return Int;
     -- function "="( Left, Right : Int ) return Boolean;
   end P;

   with P;
   procedure Test is
     A, B, C : P.Int := some_value;
   begin
     C := A + B; -- illegal reference to operator
     C:= P."+" (A,B);
     declare
       use P;
     begin
       C := A + B; -- now legal
     end;
   end Test;

====================
"use type" Clauses
====================

----------
Examples
----------

.. include:: examples/135_visibility/use_type_clauses.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/135_visibility.html#use-type-clauses`

---------------------
`use type` Clauses
---------------------

* Syntax

   .. code:: Ada

      use_type_clause ::= use type subtype_mark
                                         {, subtype_mark};

* Makes operators directly visible for specified type

   - Implicit and explicit operator function declarations
   - Only those that mention the type in the profile

      + Parameters and/or result type

* More specific alternative to :ada:`use` clauses

   - Especially useful when multiple :ada:`use` clauses introduce ambiguity

---------------------------
`use type` Clause Example
---------------------------

.. code:: Ada

   package P is
     type Int is range Lower .. Upper;
     -- implicit declarations
     -- function "+"( Left, Right : Int ) return Int;
     -- function "="( Left, Right : Int ) return Boolean;
   end P;
   with P;
   procedure Test is
     A, B, C : P.Int := some_value;
     use type P.Int;
     D : Int; -- not legal
   begin
     C := A + B; -- operator is visible
   end Test;

--------------------------------------
`use Type` Clauses and Multiple Types
--------------------------------------

* One clause can make ops for several types visible

   - When multiple types are in the profiles

* No need for multiple clauses in that case

.. code:: Ada

   package P is
     type Miles_T is digits 6;
     type Hours_T is digits 6;
     type Speed_T is digits 6;
     -- "use type" on any of Miles_T, Hours_T, Speed_T
     -- makes operator visible
     function "/"( Left : Miles_T;
                   Right : Hours_T )
                   return Speed_T;
   end P;

-----------------------------
Multiple `use type` Clauses
-----------------------------

* May be necessary
* Only those that mention the type in their profile are made visible

.. code:: Ada

   package P is
     type T1 is range 1 .. 10;
     type T2 is range 1 .. 10;
     -- implicit
     -- function "+"( Left : T2; Right : T2 ) return T2;
     type T3 is range 1 .. 10;
     -- explicit
     function "+"( Left : T1; Right : T2 ) return T3;
   end P;

   with P;
   procedure UseType is
     X1 : P.T1;
     X2 : P.T2;
     X3 : P.T3;
     use type P.T1;
   begin
     X3 := X1 + X2; -- operator visible because it uses T1
     X2 := X2 + X2; -- operator not visible
   end UseType;

========================
"use all type" Clauses
========================

----------
Examples
----------

.. include:: examples/135_visibility/use_all_type_clauses.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/135_visibility.html#use-all-type-clauses`

-------------------------
`use all type` Clauses
-------------------------

.. admonition:: Language Variant

   Ada 2012

* Makes all primitive operations for the type visible

   - Not just operators
   - Especially, subprograms that are not operators

* Still need a :ada:`use` clause for other entities

   - Typically exceptions

-------------------------------
`use all type` Clause Example
-------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   package Complex is
     type Number is private;
     function "+" (Left, Right : Number) return Number;
     procedure Make ( C : out Number;
                      From_Real, From_Imag : Float );
   ...

.. code:: Ada

   with Complex;
   use all type Complex.Number;
   procedure Demo is
     A, B, C : Complex.Number;
     procedure Non_Primitive ( X : Complex.Number ) is null;
   begin
     -- "use all type" makes these available
     Make (A, From_Real => 1.0, From_Imag => 0.0);
     Make (B, From_Real => 1.0, From_Imag => 0.0);
     C := A + B;
     -- but not this one
     Non_Primitive (0);
   end Demo;

--------------------------------------
`use all type` v. `use type` Example
--------------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   with Complex;   use type Complex.Number;
   procedure Demo is
     A, B, C : Complex.Number;
   Begin
     -- these are always allowed
     Complex.Make (A, From_Real => 1.0, From_Imag => 0.0);
     Complex.Make (B, From_Real => 1.0, From_Imag => 0.0);
     -- "use type" does not give access to these
     Make (A, 1.0, 0.0); -- not visible
     Make (B, 1.0, 0.0); -- not visible
     -- but this is good
     C := A + B;
     Complex.Put (C);
     -- this is not allowed
     Put (C); -- not visible
   end Demo;

===================
Renaming Entities
===================

---------------------------------
Three Positives Make a Negative
---------------------------------

* Good Coding Practices ...

   - Descriptive names
   - Modularization
   - Subsystem hierarchies

* Can result in cumbersome references

   .. code:: Ada

      -- use cosine rule to determine distance between two points,
      -- given angle and distances between observer and 2 points
      -- A**2 = B**2 + C**2 - 2*B*C*cos(A)
      Observation.Sides (Viewpoint_Types.Point1_Point2) :=
        Math_Utilities.Square_Root
          (Observation.Sides (Viewpoint_Types.Observer_Point1)**2 +
           Observation.Sides (Viewpoint_Types.Observer_Point2)**2 +
           2.0 * Observation.Sides (Viewpoint_Types.Observer_Point1) *
             Observation.Sides (Viewpoint_Types.Observer_Point2) *
             Math_Utilities.Trigonometry.Cosine
               (Observation.Vertices (Viewpoint_Types.Observer)));

--------------------------------
Writing Readable Code - Part 1
--------------------------------

* We could use :ada:`use` on package names to remove some dot-notation

   .. code:: Ada

      -- use cosine rule to determine distance between two points, given angle
      -- and distances between observer and 2 points A**2 = B**2 + C**2 -
      -- 2*B*C*cos(A)
      Observation.Sides (Point1_Point2) :=
        Square_Root
          (Observation.Sides (Observer_Point1)**2 +
           Observation.Sides (Observer_Point2)**2 +
           2.0 * Observation.Sides (Observer_Point1) *
             Observation.Sides (Observer_Point2) *
             Cosine (Observation.Vertices (Observer)));

* But that only shortens the problem, not simplifies it

   - If there are multiple "use" clauses in scope:

      + Reviewer may have hard time finding the correct definition
      + Homographs may cause ambiguous reference errors

* We want the ability to refer to certain entities by another name (like an alias) with full read/write access (unlike temporary variables)

-----------------------
The `renames` Keyword
-----------------------

* Certain entities can be renamed within a declarative region

   - Packages

      .. code:: Ada

         package Trig renames Math.Trigonometry

   - Objects (or elements of objects)

      .. code:: Ada

         Angles : Viewpoint_Types.Vertices_Array_T
                  renames Observation.Vertices;
         Required_Angle : Viewpoint_Types.Vertices_T
                  renames Viewpoint_Types.Observer;

   - Subprograms

      .. code:: Ada

         function Sqrt (X : Base_Types.Float_T)
                        return Base_Types.Float_T
                        renames Math.Square_Root;

--------------------------------
Writing Readable Code - Part 2
--------------------------------

* With :ada:`renames` our complicated code example is easier to understand

   .. code:: Ada

      begin
         package Math renames Math_Utilities;
         package Trig renames Math.Trigonometry;

         function Sqrt (X : Base_Types.Float_T) return Base_Types.Float_T
           renames Math.Square_Root;

         Side1          : Base_Types.Float_T
           renames Observation.Sides (Viewpoint_Types.Observer_Point1);
         -- Rename the others as Side2, Angles, Required_Angle, Desired_Side
      begin
         ...
         -- use cosine rule to determine distance between two points, given angle
         -- and distances between observer and 2 points A**2 = B**2 + C**2 -
         -- 2*B*C*cos(A)
         Desired_Side := Sqrt (Side1**2 + Side2**2 +
                               2.0 * Side1 * Side2 * Trig.Cosine (Angles (Required_Angle)));
      end;

========
Lab
========

.. include:: labs/135_visibility.lab.rst

=========
Summary
=========

---------
Summary
---------

.. admonition:: Language Variant

   Ada 2012

* :ada:`use` clauses are not evil but can be abused

   - Can make it difficult for others to understand code

* :ada:`use all type` clauses are more likely in practice than :ada:`use type` clauses

   - Only available in Ada 2012 and later

* :ada:`Renames` allow us to alias entities to make code easier to read

   - Subprogram renaming has many other uses, such as adding / removing default parameter values
