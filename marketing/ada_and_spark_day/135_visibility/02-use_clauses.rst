===============
"use" Clauses
===============

---------------
"use" Clauses
---------------

* **use Utilities;** provides direct visibility into public items in **Utilities**

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
