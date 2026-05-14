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

---------------------------------
Typical Use for Null Procedures
---------------------------------

* Used to override a procedure from a base type

  .. code:: Ada
    :number-lines: 1
    :font-size: footnotesize

    type One_T is tagged record
       Field : Integer;
    end record;
    procedure Print (R : One_T);

    type Two_T is new One_T with null record;
    procedure Print (R : Two_T) is null;

    One : One_T := (Field => 123);
    Two : Two_T := (Field => 345);

  * :ada:`Two.Print` basically does nothing (because of line 7)
  * If line 7 is commented out, :ada:`Two.Print` would go to line 4

* Used as a placeholder to get your code to build

  .. code:: Ada
    :font-size: footnotesize

     procedure Step_One is null;
     procedure Step_Two is null;
     procedure Step_Three is null;
   
     procedure Do_Something is
     begin
        Step_One;
        Step_Two;
        Step_Three;
     end Do_Something;

  * :ada:`Do_Something` will build and run successfully

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

