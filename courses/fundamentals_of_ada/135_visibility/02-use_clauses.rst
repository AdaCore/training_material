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

