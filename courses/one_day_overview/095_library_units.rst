****************
Library Units
****************

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

===============
Library Units
===============

---------------
Library Units
---------------

.. code:: Ada

   package Operating_System is
     procedure Foo(...);
     procedure Bar(...);
     package Process_Manipulation is
       ...
     end Process_Manipulation;
     package File_System is
       ...
     end File_System;
   end Operating_System;

* `Operating_System` is library unit
* `Foo`, `Bar`, etc - not library units

-----------------------------
Objects In Library Packages
-----------------------------

* Exist as long as program executes (i.e., "forever")

.. code:: Ada

   package Named_Common is
     X : Integer; -- valid object for life of application
     Y : Float;    -- valid object for life of application
   end Named_Common;
--------------------------
Library Unit Subprograms
--------------------------

* Specifications in declaration and body must conform

   - Example

      + Spec for P

      .. code:: Ada

         procedure P (F : in integer);

      + Body for P

      .. code:: Ada

         procedure P (F : in float) is
         begin
         ...
         end P;

   - Declaration creates subprogram `P` in library
   - Declaration exists so body does not act as declaration
   - Compilation of file "p.adb" must fail

* New declaration with same name replaces old one
* Thus cannot overload library units

------------------
Main Subprograms
------------------

* Must be library subprograms
* No special program unit name required
* Can be many per program library
* Always can be procedures
* Can be functions if implementation allows it

   - Execution environment must know how to handle result

.. code:: Ada

   with Ada.Text_IO;
   procedure Hello is
   begin
     Ada.Text_IO.Put("Hello World");
   end Hello;

================
Dependencies
================

-----------------
 `with` Clauses
-----------------

* Specify the library units that a compilation unit depends upon

   - The "context" in which the unit is compiled

* Syntax (simplified)

   .. code:: Ada

      context_clause ::= { context_item }
      context_item ::= with_clause | use_clause
      with_clause ::= with library_unit_name
                      { , library_unit_name };

.. code:: Ada

   with Ada.Text_IO; -- dependency
   procedure Hello is
   begin
     Ada.Text_IO.Put ("Hello World");
   end Hello;

----------------
What To Import
----------------

* Need only name direct dependencies

   - Those actually referenced in the corresponding unit

* Will not cause compilation of referenced units

   - Unlike "include directives" of some languages

.. code:: Ada

   package A is
     type Something is ...
   end A;

   with A;
   package B is
     type Something is record
       Field : A.Something;
     end record;
   end B;

   with B; -- no "with" of A
   procedure Foo is
     X : B.Something;
   begin
     X.Field := ...

