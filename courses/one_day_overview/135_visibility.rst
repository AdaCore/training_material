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

----------
Examples
----------

.. include:: examples/135_visibility/use_clauses.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/135_visibility.html#use-clauses`

----------------
 `use` Clauses
----------------

* Provide direct visibility into packages' exported items

   + :dfn:`Direct Visibility` - as if object was referenced from within package being used

* May still use expanded name

.. code:: Ada

   package Ada.Text_IO is
     procedure Put_Line(...);
     procedure New_Line(...);
     ...
   end Ada.Text_IO;

   with Ada.Text_IO;
   procedure Hello is
     use Ada.Text_IO;
   begin
     Put_Line("Hello World");
     New_Line(3);
     Ada.Text_IO.Put_Line ("Good bye");
   end Hello;

---------------------
`use` Clause Syntax
---------------------

* May have several, like :ada:`with` clauses
* Can refer to any visible package (including nested packages)
* Syntax

   .. code:: Ada

      use_package_clause ::= use package_name {, package_name};

* Can only :ada:`use` a package

   - Subprograms have no contents to :ada:`use`

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
     function F return Integer is (Constant_A + Constant_B);
   end P;

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
     -- function "+"(Left, Right : Int) return Int;
     -- function "="(Left, Right : Int) return Boolean;
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
