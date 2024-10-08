************
Visibility
************

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

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
         Position => Messages.Queue.Front);

* Operators treated as functions defeat the purpose of overloading

   .. code::

      Complex1 := Complex_Types."+" (Complex2, Complex3);

* Ada has mechanisms to simplify hierarchies

--------------------------
Operators and Primitives
--------------------------

* :dfn:`Operators`

   - Constructs which behave generally like functions but which differ syntactically or semantically
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

---------------
"use" Clauses
---------------

* :ada:`use Pkg;` provides direct visibility into public items in :ada:`Pkg`

   + :dfn:`Direct Visibility` - as if object was referenced from within package being used
   + :dfn:`Public Items` - any entity defined in package spec public section

* May still use expanded name

.. code:: Ada

   package Ada.Text_IO is
     procedure Put_Line (...);
     procedure New_Line (...);
     ...
   end Ada.Text_IO;

   with Ada.Text_IO;
   procedure Hello is
     use Ada.Text_IO;
   begin
     Put_Line ("Hello World");
     New_Line (3);
     Ada.Text_IO.Put_Line ("Good bye");
   end Hello;

---------------------
"use" Clause Syntax
---------------------

* May have several, like :ada:`with` clauses
* Can refer to any visible package (including nested packages)
* Syntax

   .. code:: Ada

      use_package_clause ::= use package_name {, package_name};

* Can only :ada:`use` a package

   - Subprograms have no contents to :ada:`use`

--------------------
"use" Clause Scope
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
     function F return Integer is (Constant_A + Constant_B);
   end P;

--------------------
No Meaning Changes
--------------------

* A new :ada:`use` clause won't change a program's meaning!
* Any directly visible names still refer to the original entities

.. code:: Ada

   package D is
     T : Float;
   end D;

   with D;
   procedure P is
     procedure Q is
       T, X : Float;
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
"use" Clauses and Child Units
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

   with Parent;
   with Parent.Child; use Parent.Child;
   procedure Demo is
     D1 : Integer := Parent.P1;
     D2 : Integer := Parent.Child.PC1;
     use Parent;
     D3 : Integer := P1; -- illegal
     D4 : Integer := PC1;
     ...

.. container:: speakernote

   D4 has access to CHILD because PARENT is "use"d

----------------------------------------
"use" Clause and Implicit Declarations
----------------------------------------

* Visibility rules apply to implicit declarations too

.. code:: Ada

   package P is
     type Int is range Lower .. Upper;
     -- implicit declarations
     -- function "+"(Left, Right : Int) return Int;
     -- function "="(Left, Right : Int) return Boolean;
   end P;

   with P;
   procedure Test is
     A, B, C : P.Int := some_value;
   begin
     C := A + B; -- illegal reference to operator
     C := P."+" (A,B);
     declare
       use P;
     begin
       C := A + B; -- now legal
     end;
   end Test;

=======================================
"use type" and "use all type" Clauses
=======================================

-------------------------------
"use type" and "use all type"
-------------------------------

* :ada:`use type` makes **primitive operators** directly visible for specified type

   - Implicit and explicit operator function declarations

   .. code:: Ada

      use type subtype_mark {, subtype_mark};

* :ada:`use all type` makes primitive operators **and all other operations** directly visible for specified type

   - All **enumerated type values** will also be directly visible

   .. code:: Ada

      use all type subtype_mark {, subtype_mark};

* More specific alternative to :ada:`use` clauses

   - Especially useful when multiple :ada:`use` clauses introduce ambiguity

*Note that* :ada:`use all type` *was introduced in Ada 2012*

--------------
Example Code
--------------

.. code:: Ada

   package Types is
     type Distance_T is range 0 .. Integer'Last;

     -- explicit declaration
     -- (we don't want a negative distance)
     function "-" (Left, Right : Distance_T)
                   return Distance_T;

     -- implicit declarations (we get the division operator
     -- for "free", showing it for completeness)
     -- function "/" (Left, Right : Distance_T) return
     --               Distance_T;

     -- primitive operation
     function Min (A, B : Distance_T)
                   return Distance_T;

   end Types;

--------------------------
"use" Clauses Comparison
--------------------------

.. image:: use_clause_comparison.png

-----------------------------
Multiple "use type" Clauses
-----------------------------

* May be necessary
* Only those that mention the type in their profile are made visible

.. code:: Ada

   package P is
     type T1 is range 1 .. 10;
     type T2 is range 1 .. 10;
     -- implicit
     -- function "+"(Left : T2; Right : T2) return T2;
     type T3 is range 1 .. 10;
     -- explicit
     function "+"(Left : T1; Right : T2) return T3;
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
      -- A**2 = B**2 + C**2 - 2*B*C*cos(angle)
      Observation.Sides (Viewpoint_Types.Point1_Point2) :=
        Math_Utilities.Square_Root
          (Observation.Sides (Viewpoint_Types.Observer_Point1)**2 +
           Observation.Sides (Viewpoint_Types.Observer_Point2)**2 -
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
      -- 2*B*C*cos(angle)
      Observation.Sides (Point1_Point2) :=
        Square_Root
          (Observation.Sides (Observer_Point1)**2 +
           Observation.Sides (Observer_Point2)**2 -
           2.0 * Observation.Sides (Observer_Point1) *
             Observation.Sides (Observer_Point2) *
             Cosine (Observation.Vertices (Observer)));

* But that only shortens the problem, not simplifies it

   - If there are multiple "use" clauses in scope:

      + Reviewer may have hard time finding the correct definition
      + Homographs may cause ambiguous reference errors

* We want the ability to refer to certain entities by another name (like an alias) with full read/write access (unlike temporary variables)

-----------------------
The "renames" Keyword
-----------------------

* :ada:`renames` declaration creates an alias to an entity

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

   - Executable code is very close to the specification
   - Declarations as "glue" to the implementation details

   .. code:: Ada

      begin
         package Math renames Math_Utilities;
         package Trig renames Math.Trigonometry;

         function Sqrt (X : Base_Types.Float_T) return Base_Types.Float_T
           renames Math.Square_Root;
         function Cos ...

         B : Base_Types.Float_T
           renames Observation.Sides (Viewpoint_Types.Observer_Point1);
         -- Rename the others as Side2, Angles, Required_Angle, Desired_Side
      begin
         ...
         -- A**2 = B**2 + C**2 - 2*B*C*cos(angle)
         A := Sqrt (B**2 + C**2 - 2.0 * B * C * Cos (Angle));
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

* :ada:`use` clauses are not evil but can be abused

   - Can make it difficult for others to understand code

* :ada:`use all type` clauses are more likely in practice than :ada:`use type` clauses

   - Only available in Ada 2012 and later

* :ada:`Renames` allow us to alias entities to make code easier to read

   - Subprogram renaming has many other uses, such as adding / removing default parameter values
