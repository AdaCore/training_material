===============
"use" Clauses
===============

---------------
"use" Clauses
---------------

* :ada:`use Utilties;` provides direct visibility into public items in :ada:`Utilties`

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

**Syntax**

.. container:: source_include 135_visibility/syntax.bnf :start-after:use_clause_syntax_begin :end-before:use_clause_syntax_end :code:bnf

* Can only :ada:`use` a package

   - Subprograms have no contents to :ada:`use`

--------------------
"use" Clause Scope
--------------------

* Applies to end of body, from first occurrence

.. code:: Ada

   package Distance_Pkg is
      Distance : Float := 12.34;
   end Distance_Pkg;

   package Time_Pkg is
      Time : Float := 98.76;
   end Time_Pkg;

   with Distance_Pkg;
   with Time_Pkg;
   use Distance_Pkg; -- everything in Distance_Pkg is now visible
   package Speed_Pkg is
      Clicks  : Float := Distance / 1.6; -- OK
      Bad     : Float := Time / 60.0;    -- compile error
      use Time_Pkg; -- everything in Time_Pkg is now visible
      Seconds : Float := Time / 60.0;    -- OK
      function Speed return Float;
   end Speed_Pkg;

   package body Speed_Pkg is
     -- all of Distance_Pkg and Time_Pkg is visible here
     function Speed return Float is (Distance / Time);
   end Speed_Pkg;

--------------------
No Meaning Changes
--------------------

* A new :ada:`use` clause won't change a program's meaning!
* Any directly visible names still refer to the original entities

.. code:: Ada

   package Distance_Pkg is
     Distance : Float;
   end Distance_Pkg;

   with Distance_Pkg;
   procedure Example is
     Distance, Miles : Float;
   begin
     declare
       use Distance_Pkg;
     begin
       -- With or without the clause, "Distance" means Q.Distance
       Miles := Distance;
     end;
   end Example;

--------------
No Ambiguity 
--------------

.. code:: Ada

   package Miles_Pkg is
     Distance : Float;
   end Miles_Pkg;

   package Kilometers_Pkg is
     Distance : Float;
   end Kilometers_Pkg;

   with Miles_Pkg, Kilometers_Pkg;
   procedure Example is
      use Miles_Pkg, Kilometers_Pkg;
      Miles      : Float;
      Kilometers : Float;
   begin
      Miles := Distance;                     -- compile error
      Kilometers := Kilometers_Pkg.Distance; -- OK
   end Example;

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
     D3 : Integer := P1;
     D4 : Integer := PC1;
     ...

.. container:: speakernote

   D4 has access to CHILD because PARENT is "use"d

----------------------------------------
"use" Clause and Implicit Declarations
----------------------------------------

* Visibility rules apply to implicit declarations too

.. code:: Ada

   package Types_Pkg is
     type Int is range Lower .. Upper;
     -- implicit declarations
     -- function "+"(Left, Right : Int) return Int;
     -- function "="(Left, Right : Int) return Boolean;
   end Types_Pkg;

   with Types_Pkg;
   procedure Test is
     A, B, C : Types_Pkg.Int := some_value;
   begin
     C := A + B; -- illegal reference to operator
     C := Types_Pkg."+" (A,B);
     declare
       use Types_Pkg;
     begin
       C := A + B; -- now legal
     end;
   end Test;

