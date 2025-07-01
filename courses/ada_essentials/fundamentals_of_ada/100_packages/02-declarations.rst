==============
Declarations
==============

----------------------
Package Declarations
----------------------

* Required in all cases

   - Cannot have a package without the declaration

* Describe the client's interface

   - Declarations are exported to clients
   - Effectively the "pin-outs" for the black-box

* When changed, requires clients recompilation

   - The "pin-outs" have changed

.. code:: Ada

   package Float_Stack is
     Max : constant := 100;
     procedure Push (X : in Float);
     procedure Pop (X : out Float);
   end Float_Stack;

   package Data is
      Object : Integer;
   end Data;

---------------------------------
Compile-Time Visibility Control
---------------------------------

* Items in the declaration are visible to users

   .. code:: Ada

      package Some_Package is
        -- exported declarations of
        --   types, variables, subprograms ...
      end Some_Package;

* Items in the body are never externally visible

   - Compiler prevents external references

   .. code:: Ada

      package body Some_Package is
        -- hidden declarations of
        --   types, variables, subprograms ...
        -- implementations of exported subprograms etc.
      end Some_Package;

---------------------------------
Example of Exporting to Clients
---------------------------------

* Variables, types, exception, subprograms, etc.

   - The primary reason for separate subprogram declarations

.. code:: Ada

   package P is
      procedure This_Is_Exported;
   end P;

   package body P is
      procedure Not_Exported is
         ...
      procedure This_Is_Exported is
         ...
   end P;

