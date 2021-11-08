****************
Library Units
****************

.. include:: support_files/symbols.rst

==============
Introduction
==============

------------
Modularity
------------

* Ability to split large system into subsystems
* Each subsystem can have its own components
* And so on ...

===============
Library Units
===============

----------
Examples
----------

.. include:: examples/095_library_units/library_units.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/095_library_units.html#library-units`

---------------
Library Units
---------------

* Those not nested within another program unit
* Candidates

   - Subprograms
   - Packages
   - Generic Units
   - Generic Instantiations
   - Renamings

* Restrictions

   - No library level tasks

      + They are always nested within another unit

   - No overloading at library level
   - No library level functions named as operators

---------------
Library Units
---------------

.. code:: Ada

   package Operating_System is
     procedure Foo( ... );
     procedure Bar( ... );
     package Process_Manipulation is
       ...
     end Process_Manipulation;
     package File_System is
       ...
     end File_System;
   end Operating_System;

* `Operating_System` is library unit
* `Foo`, `Bar`, etc - not library units

---------------------------
No 'Object' Library Items
---------------------------

.. code:: Ada

   package Library_Package is
     ...
   end Library_Package;

   -- Illegal: no such thing as "file scope"
   Library_Object : Integer;

   procedure Library_Procedure;

   function Library_Function (Formal : in out Integer) is
     Local : Integer;
   begin
     ...
   end Library_Function;

-----------------------------
Declared Object "Lifetimes"
-----------------------------

* Same as their enclosing declarative region

   - Objects are always declared within some declarative region

* No ``static`` etc. directives as in C
* Objects declared within any subprogram

   - Exist only while subprogram executes

   .. code:: Ada

          procedure Library_Subprogram is
            X : Integer;
            Y : Float;
          begin
            ...
          end Library_Subprogram;

-----------------------------
Objects In Library Packages
-----------------------------

* Exist as long as program executes (i.e., "forever")

.. code:: Ada

   package Named_Common is
     X : Integer; -- valid object for life of application
     Y : Float;    -- valid object for life of application
   end Named_Common;

---------------------------------
Objects In Non-library Packages
---------------------------------

* Exist as long as region enclosing the package

.. code:: Ada

   procedure P is
     X : Integer; -- available while in P and Inner
     package Inner is
       Z : Boolean; -- available while in Inner
     end Inner;
     Y : Real; -- available while in P
   begin
     ...
   end P;

--------------------
Program "Lifetime"
--------------------

* Run-time library is initialized
* All (any) library packages are elaborated

   - Declarations in package declarative part are elaborated
   - Declarations in package body declarative part are elaborated
   - Executable part of package body is executed (if present)

* Main program's declarative part is elaborated
* Main program's sequence of statements executes
* Program executes until all threads terminate
* All objects in library packages cease to exist
* Run-time library shuts down

--------------------------
Library Unit Subprograms
--------------------------

* Recall: separate declarations are optional

   - Body can act as declaration if no declaration provided

* Separate declaration provides usual benefits

   - Changes/recompilation to body only require relinking clients

* File 1 (p.ads for GNAT)

   .. code:: Ada

      procedure P (F : in Integer);

* File 2 (p.adb for GNAT)

   .. code:: Ada

      procedure P (F : in Integer) is
      begin
        ...
      end P;

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
     Ada.Text_IO.Put( "Hello World" );
   end Hello;

================
Dependencies
================

----------
Examples
----------

.. include:: examples/095_library_units/with_clauses.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/095_library_units.html#with-clauses`

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

-----------------------
`with` Clauses Syntax
-----------------------

* Helps explain restrictions on library units

   - No overloaded library units
   - If overloading allowed, which `P` would :ada:`with P;` refer to?
   - No library unit functions names as operators

      + Mostly because of no overloading

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

=========
Summary
=========

---------
Summary
---------

* Library Units are "standalone" entities

   - Can contain subunits with similar structure

* :ada:`with` clauses interconnect library units

   - Express dependencies of the one being compiled
   - Not textual inclusion!
