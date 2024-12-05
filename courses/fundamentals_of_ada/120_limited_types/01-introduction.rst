==============
Introduction
==============

-------
Views
-------

* Specify how values and objects may be manipulated
* Are implicit in much of the language semantics

   - Constants are just variables without any assignment view
   - Task types, protected types implicitly disallow assignment
   - Mode :ada:`in` formal parameters disallow assignment

.. code:: Ada

   Variable : Integer := 0;
   ...
   -- P's view of X prevents modification
   procedure P(X :  in  Integer) is
   begin
       ...
   end P;
   ...
   P(Variable);

-------------------------------
Limited Type Views' Semantics
-------------------------------

* Prevents copying via predefined assignment

   - Disallows assignment between objects
   - Must make your own `copy` procedure if needed

   .. code:: Ada

      type File is limited ...
      ...
      F1, F2 : File;
      ...
      F1 := F2; -- compile error

* Prevents incorrect comparison semantics

   - Disallows predefined equality operator
   - Make your own equality function `=` if needed

-------------------------------
Inappropriate Copying Example
-------------------------------

.. code:: Ada

   type File is ...
   F1, F2 : File;
   ...
   Open (F1);
   Write (F1, "Hello");
   -- What is this assignment really trying to do?
   F2 := F1;

-----------------------------
Intended Effects of Copying
-----------------------------

.. code:: Ada

   type File is ...
   F1, F2 : File;
   ...
   Open (F1);
   Write (F1, "Hello");
   Copy (Source => F1, Target => F2);

