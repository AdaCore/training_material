=================
Null Procedures
=================

-----------------------------
Null Procedure Declarations
-----------------------------

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

..
  language_version 2005

--------------------------------
Null Procedures As Completions
--------------------------------

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

..
  language_version 2005

--------------------------------------
Typical Use for Null Procedures: OOP
--------------------------------------

* When you want a method to be concrete, rather than abstract, but don't have anything for it to do

   - The method is then always callable, including places where an abstract routine would not be callable
   - More convenient than full null-body definition

..
  language_version 2005

------------------------
Null Procedure Summary
------------------------

* Allowed where you can have a full body

   - Syntax is then for shorthand for a full null-bodied procedure

* Allowed where you can have a declaration!

   - Example: package declarations
   - Syntax is shorthand for both declaration and completion

      + Thus no body required/allowed

* Formal parameters are allowed

.. code:: Ada

   procedure Do_Something (P : in     Integer) is null;

..
  language_version 2005

